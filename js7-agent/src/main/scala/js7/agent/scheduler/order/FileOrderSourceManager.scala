package js7.agent.scheduler.order

import cats.instances.vector._
import cats.syntax.foldable._
import cats.syntax.parallel._
import cats.syntax.traverse._
import com.typesafe.config.Config
import java.nio.file.StandardWatchEventKinds.{ENTRY_CREATE, ENTRY_DELETE}
import java.nio.file.{Path, Paths}
import js7.agent.data.AgentState
import js7.agent.data.ordersource.FileOrderSourceState
import js7.agent.scheduler.order.FileOrderSourceManager._
import js7.base.io.file.watch.DirectoryEvent.{FileAdded, FileDeleted, FileModified}
import js7.base.io.file.watch.{DirectoryEvent, DirectoryWatcher, WatchOptions}
import js7.base.log.Logger
import js7.base.monixutils.AsyncMap
import js7.base.problem.Checked
import js7.base.thread.IOExecutor
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax._
import js7.data.event.KeyedEvent.NoKey
import js7.data.item.SimpleItemEvent.SimpleItemAttachedToAgent
import js7.data.item.SimpleItemId
import js7.data.ordersource.FileOrderSource.FileArgumentName
import js7.data.ordersource.OrderSourceEvent.{OrderSourceOrderArised, OrderSourceOrderVanished}
import js7.data.ordersource.{FileOrderSource, OrderSourceEvent, OrderSourceId, SourceOrderName}
import js7.data.value.{NamedValues, StringValue}
import js7.journal.state.{JournaledStatePersistence, LockKeeper}
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import monix.reactive.subjects.PublishSubject
import scala.concurrent.duration.Deadline.now

/** Persists, recovers and runs FileOrderSources. */
final class FileOrderSourceManager(
  persistence: JournaledStatePersistence[AgentState],
  config: Config)
  (implicit scheduler: Scheduler, iox: IOExecutor)
{
  private val watchDelay = config.getDuration("js7.fileordersource.watch-delay").toFiniteDuration
  private val lockKeeper = new LockKeeper[SimpleItemId]
  private val idToStopper = AsyncMap(Map.empty[OrderSourceId, Task[Unit]])

  def stop: Task[Unit] =
    idToStopper.removeAll
      .flatMap(_.values.toVector.parUnorderedSequence)
      .map(_.unorderedFold)

  def start(): Task[Unit] = {
    persistence.currentState
      .map(_.fileOrderSourcesState)
      .map(fileOrderSourcesState => fileOrderSourcesState.idToFileOrderSource.values)
      .flatMap(_
        .toVector
        .traverse(startObserving)
        .map(_.foldMap(identity)))
  }

  def update(orderSource: FileOrderSource): Task[Checked[Unit]] =
    lockKeeper.lock(orderSource.id) {
      persistence
        // TODO No Transaction required here
        .persistTransaction[SimpleItemAttachedToAgent](NoKey)(agentState =>
          Right(
            !agentState.fileOrderSourcesState.contains(orderSource) thenList
              SimpleItemAttachedToAgent(orderSource)))
        .flatMapT { case (_, agentState) =>
          startObserving(agentState.fileOrderSourcesState.idToFileOrderSource(orderSource.id))
            .map(Right(_))
        }
    }

  private def startObserving(fosState: FileOrderSourceState): Task[Unit] =
    Task.defer {
      val stop = PublishSubject[Unit]()
      val observable = observeFileOrderSource(fosState, stop).memoize

      // Execute previously registered stopper (which awaits completion),
      // and start our observable as a fiber.
      // At the same time, register a stopper in idToStopper.
      // The stopper is a task that stops the observable and awaits its completion.
      idToStopper
        .update(fosState.id, previous =>
          // Wait for previous Observable to complete (stop and fiber.join)
          previous.getOrElse(Task.unit) >>
            observable.start
              .map(fiber =>
                // The delayed task to register for the next update:
                Task.defer {
                  stop.onComplete()
                  fiber.join
                }))
        .as(())
    }

  private def observeFileOrderSource(orderSourceState: FileOrderSourceState, stop: Observable[Unit])
  : Task[Unit] = {
    import orderSourceState.{directoryState, fileOrderSource}

    val directoryToEvents =
      DirectoryWatcher
        .observe(
          directoryState,
          WatchOptions(
            Paths.get(fileOrderSource.directory),
            Set(ENTRY_CREATE, ENTRY_DELETE),
            delay = watchDelay))
        .takeUntil(stop)
        .map(directoryEventsToPersistableEvents(fileOrderSource, _))
        .mapEval(events =>
          // TODO Transaction not required
          persistence.persistTransaction[OrderSourceEvent](fileOrderSource.id)(_ => Right(events)))
        .foreachL {
          case Left(problem) => logger.error(problem.toString)
          case Right(_) =>
        }

    def loop: Task[Unit] =
      Task.defer {
        val since = now
        directoryToEvents
          .onErrorHandleWith { throwable =>
            logger.error(throwable.toStringWithCauses)
            for (t <- throwable.ifNoStackTrace) logger.debug(t.toString, t)
            loop.delayExecution((since + DirectoryWatcher.hotLoopBrake).elapsedOrZero)
          }
      }

    loop
  }

  private def directoryEventsToPersistableEvents(
    fileOrderSource: FileOrderSource,
    directoryEvents: Seq[DirectoryEvent])
  : Seq[OrderSourceEvent] = {
    val directory = Paths.get(fileOrderSource.directory)
    directoryEvents.flatMap {
      case FileAdded(path) =>
        OrderSourceOrderArised(SourceOrderName(path.toString), toOrderArguments(directory, path)) :: Nil

      case FileDeleted(path) =>
        OrderSourceOrderVanished(SourceOrderName(path.toString)) :: Nil

      case FileModified(_) =>
        sys.error("Unexpected FileModified")
    }
  }

  private def toOrderArguments(directory: Path, path: Path) =
    NamedValues(FileArgumentName -> StringValue(directory.resolve(path).toString))
}

object FileOrderSourceManager
{
  private val logger = Logger(getClass)
}
