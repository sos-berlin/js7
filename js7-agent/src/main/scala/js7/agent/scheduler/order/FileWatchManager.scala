package js7.agent.scheduler.order

import cats.instances.vector._
import cats.syntax.foldable._
import cats.syntax.monoid._
import cats.syntax.parallel._
import cats.syntax.traverse._
import com.typesafe.config.Config
import java.nio.file.StandardWatchEventKinds.{ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY}
import java.nio.file.{Path, Paths}
import java.util.regex.Matcher
import js7.agent.data.AgentState
import js7.agent.data.orderwatch.FileWatchState
import js7.agent.scheduler.order.FileWatchManager._
import js7.base.io.file.watch.DirectoryEvent.{FileAdded, FileDeleted}
import js7.base.io.file.watch.DirectoryEventDelayer.syntax.RichDelayLineObservable
import js7.base.io.file.watch.{DirectoryWatcher, WatchOptions}
import js7.base.log.Logger
import js7.base.monixutils.AsyncMap
import js7.base.problem.Checked
import js7.base.thread.IOExecutor
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.time.ScalaTime._
import js7.base.utils.Collections.implicits.RichIterableOnce
import js7.base.utils.LockKeeper
import js7.base.utils.ScalaUtils.syntax._
import js7.data.agent.AgentPath
import js7.data.event.KeyedEvent.NoKey
import js7.data.item.BasicItemEvent.{ItemAttachedToMe, ItemDetached}
import js7.data.order.OrderId
import js7.data.orderwatch.FileWatch.FileArgumentName
import js7.data.orderwatch.OrderWatchEvent.{ExternalOrderArised, ExternalOrderVanished}
import js7.data.orderwatch.{ExternalOrderName, FileWatch, OrderWatchPath}
import js7.data.value.expression.Expression
import js7.data.value.expression.scopes.{EnvScope, NowScope}
import js7.data.value.{NamedValues, StringValue}
import js7.journal.state.StatePersistence
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import monix.reactive.subjects.PublishSubject
import scala.concurrent.duration.Deadline.now
import scala.jdk.CollectionConverters._

/** Persists, recovers and runs FileWatches. */
final class FileWatchManager(
  ownAgentPath: AgentPath,
  persistence: StatePersistence[AgentState],
  config: Config)
  (implicit scheduler: Scheduler, iox: IOExecutor)
{
  private val retryDelays = config.getDurationList("js7.filewatch.retry-delays")
    .asScala.map(_.toFiniteDuration).toVector match {
    case Vector() => 1.s :: Nil
    case o => o
  }
  private val pollTimeout = config.getDuration("js7.filewatch.poll-timeout").toFiniteDuration max 0.ms
  private val watchDelay = config.getDuration("js7.filewatch.watch-delay").toFiniteDuration max 0.s
  private val lockKeeper = new LockKeeper[OrderWatchPath]
  private val idToStopper = AsyncMap(Map.empty[OrderWatchPath, Task[Unit]])

  def stop: Task[Unit] =
    idToStopper.removeAll
      .flatMap(_.values.toVector.parUnorderedSequence)
      .map(_.unorderedFold)

  def start: Task[Checked[Unit]] =
    persistence.awaitCurrentState
      .map(_.allFileWatchesState)
      .map(_.pathToFileWatchState.values)
      .flatMap(_
        .toVector
        .traverse(startWatching)
        .map(_.fold_))

  def update(fileWatch: FileWatch): Task[Checked[Unit]] =
    lockKeeper.lock(fileWatch.path) {
      persistence
        .persist(agentState =>
          Right(
            !agentState.allFileWatchesState.contains(fileWatch) thenList
              NoKey <-: ItemAttachedToMe(fileWatch)))
        .flatMapT { case (_, agentState) =>
          startWatching(agentState.allFileWatchesState.pathToFileWatchState(fileWatch.path))
        }
    }

  def remove(orderWatchPath: OrderWatchPath): Task[Checked[Unit]] =
    lockKeeper.lock(orderWatchPath) {
      persistence
        .persist(agentState =>
          Right(
            agentState.allFileWatchesState.pathToFileWatchState.get(orderWatchPath) match {
              case None => Nil
              case Some(fileWatchState) =>
                // When a FileWatch is detached, all arisen files vanishes now,
                // to allow proper remove of the Controller's orders (after OrderFinished), and
                // to allow the Controller to move the FileWatch to a different Agent,
                // because the other Agent will start with an empty FileWatchState.
                val vanished = fileWatchState.directoryState.fileToEntry.keys.view
                  .map(file => orderWatchPath <-: ExternalOrderVanished(ExternalOrderName(file.toString)))
                (vanished ++
                  Seq(NoKey <-: ItemDetached(orderWatchPath, ownAgentPath))
                ).toVector
            }))
        .flatMapT { case (_, agentState) =>
          stopWatching(orderWatchPath)
            .map(_ => Right(()))
        }
    }

  private def startWatching(fileWatchState: FileWatchState): Task[Checked[Unit]] =
    Task.defer {
      val stop = PublishSubject[Unit]()
      watch(fileWatchState, stop)
        .traverse(observable =>
          // Execute previously registered stopper (which awaits completion),
          // and start our observable as a fiber.
          // At the same time, register a stopper in idToStopper.
          // The stopper is a task that stops the observable and awaits its completion.
          idToStopper
            .update(fileWatchState.id, previous =>
              // Wait for previous Observable to complete (stop and fiber.join)
              previous.getOrElse(Task.unit) >>
                observable.start
                  .map(fiber =>
                    // The delayed task to register for the next update:
                    Task.defer {
                      stop.onComplete()
                      fiber.join
                    }))
            // ???
            .void
            .as(Right(())))
    }

  private def stopWatching(id: OrderWatchPath): Task[Unit] =
    idToStopper
      .remove(id)
      .flatMap(_ getOrElse Task.unit)

  private def watch(fileWatchState: FileWatchState, stop: Observable[Unit]): Checked[Task[Unit]] = {
    import fileWatchState.fileWatch

    fileWatch.directory
      .eval(EnvScope)
      .flatMap(_.asString)
      .map(Paths.get(_))
      .map { directory =>
        val delayIterator = retryDelays.iterator ++ Iterator.continually(retryDelays.last)
        var directoryState = fileWatchState.directoryState

        DirectoryWatcher
          .observable(
            directoryState,
            WatchOptions(
              directory,
              Set(ENTRY_CREATE, ENTRY_MODIFY, ENTRY_DELETE),
              // Regular expresion is evaluated twice: here and when deriving OrderId
              matches = relativePath => fileWatch.resolvedPattern.matcher(relativePath.toString).matches,
              retryDelays = retryDelays,
              pollTimeout = pollTimeout,
              delay = watchDelay))
          .takeUntil(stop)
          .flatMap(Observable.fromIterable)
          .delayFileAdded(fileWatch.delay)  // buffers without limit all incoming event
          .bufferIntrospective(1024/*TODO?*/)
          .mapEval(dirEventSeqs =>
            lockKeeper.lock(fileWatch.path)(
              persistence.awaitCurrentState
                .flatMap(agentState =>
                  if (!agentState.allFileWatchesState.pathToFileWatchState.contains(fileWatch.path))
                    Task.pure(Right(Nil -> agentState))
                  else
                    persistence.persist { agentState =>
                      Right(
                        dirEventSeqs
                          .view
                          .flatten
                          // In case of DirectoryWatcher error recovery, duplicate DirectoryEvent may occur.
                          // We check this here.
                          .flatMap(dirEvent =>
                            agentState.allFileWatchesState.pathToFileWatchState
                              .get(fileWatch.path)
                              // Ignore late events after FileWatch has been removed
                              .flatMap(fileWatchState =>
                                dirEvent match {
                                  case fileAdded @ FileAdded(path) if !fileWatchState.containsPath(path) =>
                                    val maybeOrderId = pathToOrderId(fileWatch, path)
                                    if (maybeOrderId.isEmpty) logger.debug(s"Ignore $fileAdded (no OrderId)")
                                    for (orderId <- maybeOrderId) yield
                                      ExternalOrderArised(
                                        ExternalOrderName(path.toString),
                                        orderId,
                                        toOrderArguments(directory, path))

                                  case FileDeleted(path) if fileWatchState.containsPath(path) =>
                                    Some(ExternalOrderVanished(ExternalOrderName(path.toString)))

                                  case event =>
                                    logger.debug(s"Ignore $event")
                                    None
                                }))
                          .map(fileWatch.path <-: _)
                          .toVector)
                    }
                )))
          .foreachL {
            case Left(problem) => logger.error(problem.toString)
            case Right((_, agentState)) =>
              directoryState = agentState.allFileWatchesState.pathToFileWatchState(fileWatch.path).directoryState
          }
          .onErrorRestartLoop(now) { (throwable, since, restart) =>
            val delay = (since + delayIterator.next()).timeLeftOrZero
            logger.error(s"Delay ${delay.pretty} after error: ${throwable.toStringWithCauses}")
            for (t <- throwable.ifStackTrace) logger.debug(t.toString, t)
            Task.sleep(delay) >> restart(now)
          }
      }
  }

  private def pathToOrderId(fileWatch: FileWatch, relativePath: Path): Option[OrderId] =
    relativePathToOrderId(fileWatch, relativePath.toString)
      .flatMap { checkedOrderId =>
        for (problem <- checkedOrderId.left) logger.error(s"${fileWatch.path} $relativePath: $problem")
        checkedOrderId.toOption
      }

  private def toOrderArguments(directory: Path, path: Path) =
    NamedValues(FileArgumentName -> StringValue(directory.resolve(path).toString))
}

object FileWatchManager
{
  private val logger = Logger(getClass)

  def relativePathToOrderId(fileWatch: FileWatch, relativePath: String): Option[Checked[OrderId]] = {
    lazy val default = OrderId.checked(s"file:${fileWatch.path.string}:$relativePath")
    val matcher = fileWatch.resolvedPattern.matcher(relativePath)
    matcher.matches() ? {
      fileWatch.orderIdExpression match {
        case None => default
        case Some(expr) =>
          eval(fileWatch.path, expr, matcher)
            .flatMap(_.toStringValueString)
            .flatMap(OrderId.checked)
      }
    }
  }

  private def eval(orderWatchPath: OrderWatchPath, expression: Expression, matchedMatcher: Matcher) =
    expression.eval(
      FileWatchScope(orderWatchPath, matchedMatcher) |+| NowScope() |+| EnvScope)
}
