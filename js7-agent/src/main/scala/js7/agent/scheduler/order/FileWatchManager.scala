package js7.agent.scheduler.order

import cats.instances.vector._
import cats.syntax.foldable._
import cats.syntax.parallel._
import cats.syntax.traverse._
import com.typesafe.config.Config
import java.nio.file.StandardWatchEventKinds.{ENTRY_CREATE, ENTRY_DELETE}
import java.nio.file.{Path, Paths}
import java.time.format.DateTimeFormatter
import java.time.{LocalDate, ZoneId, ZonedDateTime}
import java.util.regex.Matcher
import js7.agent.data.AgentState
import js7.agent.data.orderwatch.FileWatchState
import js7.agent.scheduler.order.FileWatchManager._
import js7.base.io.file.watch.DirectoryEvent.{FileAdded, FileDeleted, FileModified}
import js7.base.io.file.watch.{DirectoryEvent, DirectoryWatcher, WatchOptions}
import js7.base.log.Logger
import js7.base.monixutils.AsyncMap
import js7.base.problem.{Checked, Problem}
import js7.base.thread.IOExecutor
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax._
import js7.data.event.KeyedEvent.NoKey
import js7.data.item.SimpleItemEvent.SimpleItemAttachedToAgent
import js7.data.item.SimpleItemId
import js7.data.order.OrderId
import js7.data.orderwatch.FileWatch.FileArgumentName
import js7.data.orderwatch.OrderWatchEvent.{ExternalOrderArised, ExternalOrderVanished}
import js7.data.orderwatch.{ExternalOrderName, FileWatch, OrderWatchEvent, OrderWatchId}
import js7.data.value.expression.Expression.{Argument, FunctionCall}
import js7.data.value.expression.{Evaluator, Expression, Scope, ValueSearch}
import js7.data.value.{NamedValues, StringValue, Value}
import js7.journal.state.{JournaledStatePersistence, LockKeeper}
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import monix.reactive.subjects.PublishSubject
import scala.concurrent.duration.Deadline.now
import scala.jdk.CollectionConverters._

/** Persists, recovers and runs FileWatches. */
final class FileWatchManager(
  persistence: JournaledStatePersistence[AgentState],
  config: Config)
  (implicit scheduler: Scheduler, iox: IOExecutor)
{
  private val retryDelays = config.getDurationList("js7.filewatch.retry-delays")
    .asScala.map(_.toFiniteDuration).toVector match {
    case Vector() => 1.s :: Nil
    case o => o
  }
  private val pollTimeout = config.getDuration("js7.filewatch.poll-timeout").toFiniteDuration max 1.s
  private val watchDelay = config.getDuration("js7.filewatch.watch-delay").toFiniteDuration min 0.s
  private val lockKeeper = new LockKeeper[SimpleItemId]
  private val idToStopper = AsyncMap(Map.empty[OrderWatchId, Task[Unit]])

  def stop: Task[Unit] =
    idToStopper.removeAll
      .flatMap(_.values.toVector.parUnorderedSequence)
      .map(_.unorderedFold)

  def start(): Task[Unit] =
    persistence.currentState
      .map(_.allFileWatchesState)
      .map(_.idToFileWatch.values)
      .flatMap(_
        .toVector
        .traverse(startWatching)
        .map(_.foldMap(identity)))

  def update(orderWatch: FileWatch): Task[Checked[Unit]] =
    lockKeeper.lock(orderWatch.id) {
      persistence
        // TODO No Transaction required here
        .persistTransaction[SimpleItemAttachedToAgent](NoKey)(agentState =>
          Right(
            !agentState.allFileWatchesState.contains(orderWatch) thenList
              SimpleItemAttachedToAgent(orderWatch)))
        .flatMapT { case (_, agentState) =>
          startWatching(agentState.allFileWatchesState.idToFileWatch(orderWatch.id))
            .map(Right(_))
        }
    }

  private def startWatching(fosState: FileWatchState): Task[Unit] =
    Task.defer {
      val stop = PublishSubject[Unit]()
      val observable = watch(fosState, stop).memoize

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

  private def watch(fileWatchState: FileWatchState, stop: Observable[Unit])
  : Task[Unit] =
    Task.defer {
      import fileWatchState.fileWatch

      val delayIterator = retryDelays.iterator ++ Iterator.continually(retryDelays.last)
      var directoryState = fileWatchState.directoryState

      DirectoryWatcher
        .observe(
          directoryState,
          WatchOptions(
            Paths.get(fileWatch.directory),
            Set(ENTRY_CREATE, ENTRY_DELETE),
            retryDelays = retryDelays,
            pollTimeout = pollTimeout,
            delay = watchDelay))
        .takeUntil(stop)
        .map(directoryEventsToPersistableEvents(fileWatch, _))
        .mapEval(events =>
          // TODO Transaction not required
          persistence.persistTransaction[OrderWatchEvent](fileWatch.id)(_ => Right(events)))
        .foreachL {
          case Left(problem) => logger.error(problem.toString)
          case Right((_, agentState)) =>
            directoryState = agentState.allFileWatchesState.idToFileWatch(fileWatch.id).directoryState
        }
        .onErrorRestartLoop(now) {
          (throwable, since, restart) =>
            val delay = (since + delayIterator.next()).timeLeftOrZero
            logger.error(s"Delay ${delay.pretty} after error: ${throwable.toStringWithCauses}")
            for (t <- throwable.ifStackTrace) logger.debug(t.toString, t)
            Task.sleep(delay) >> restart(now)
        }
  }

  private def directoryEventsToPersistableEvents(
    fileWatch: FileWatch,
    directoryEvents: Seq[DirectoryEvent])
  : Seq[OrderWatchEvent] = {
    val directory = Paths.get(fileWatch.directory)
    directoryEvents.flatMap { directoryEvent =>
      val relativePath = directoryEvent.relativePath.toString
      relativePathToOrderId(fileWatch, relativePath)
        .flatMap { checkedOrderId =>
          for (problem <- checkedOrderId.left) logger.error(s"${fileWatch.id} $relativePath: $problem")
          checkedOrderId.toOption
        }
        .map(orderId =>
          directoryEvent match {
            case FileAdded(path) =>
              ExternalOrderArised(
                ExternalOrderName(path.toString),
                orderId,
                toOrderArguments(directory, path))

            case FileDeleted(path) =>
              ExternalOrderVanished(ExternalOrderName(path.toString))

            case FileModified(_) =>
              sys.error("Unexpected FileModified")
          })
    }
  }

  private def toOrderArguments(directory: Path, path: Path) =
    NamedValues(FileArgumentName -> StringValue(directory.resolve(path).toString))
}

object FileWatchManager
{
  private val NumberRegex = "([0-9]+)".r
  private val logger = Logger(getClass)

  def relativePathToOrderId(fileWatch: FileWatch, relativePath: String)
  : Option[Checked[OrderId]] = {
    lazy val default = OrderId.checked(s"file:${fileWatch.id.string}:$relativePath")
    fileWatch.pattern match {
      case None =>
        Some(default)
      case Some(pattern) =>
        val matcher = pattern.matcher(relativePath)
        matcher.matches() ? {
          fileWatch.orderIdExpression match {
            case None => default
            case Some(expr) =>
              eval(fileWatch.id, expr, matcher)
                .flatMap(_.toStringValueString)
                .flatMap(OrderId.checked)
          }
        }
    }
  }

  private def eval(orderWatchId: OrderWatchId, expression: Expression, matcher: Matcher) =
    Evaluator.eval(expression, new FileWatchScope(orderWatchId, matcher))

  private final class FileWatchScope(orderWatchId: OrderWatchId, matcher: Matcher) extends Scope {
    import ValueSearch.{LastOccurred, Name}

    val symbolToValue = symbol => Left(Problem(s"Unknown symbol: $symbol"))

    val findValue = {
      case ValueSearch(LastOccurred, Name("yyyy-mm-dd")) =>
        Right(Some(StringValue(LocalDate.now.toString)))

      case ValueSearch(LastOccurred, Name("orderWatchId")) =>
        Right(Some(StringValue(orderWatchId.string)))

      case ValueSearch(LastOccurred, Name(NumberRegex(nr))) =>
        Checked.catchNonFatal(nr.toInt).flatMap(index =>
          if (index < 0 || index > matcher.groupCount)
            Left(Problem(s"Unknown index in regular expression group: $index"))
          else
            Right(Some(StringValue(matcher.group(index)))))

      case _ => Right(None)
    }

    override def evalFunctionCall(functionCall: Expression.FunctionCall): Checked[Value] =
      functionCall match {
        case FunctionCall("now", Seq(
          Argument(formatExpr, None | Some("format")),
          Argument(timezoneExpr, None | Some("timezone")))) =>
          for {
            format <- evaluator.eval(formatExpr).flatMap(_.toStringValueString)
            timezone <- evaluator.eval(timezoneExpr).flatMap(_.toStringValueString)
          } yield
            StringValue(
              ZonedDateTime.now
                .withZoneSameInstant(ZoneId.of(timezone))
                .format(DateTimeFormatter.ofPattern(format)))

        case _ => super.evalFunctionCall(functionCall)
      }
  }
}
