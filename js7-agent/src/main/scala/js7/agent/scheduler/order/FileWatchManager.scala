package js7.agent.scheduler.order

import cats.instances.vector.*
import cats.syntax.foldable.*
import cats.syntax.monoid.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import com.typesafe.config.Config
import java.nio.file.{Path, Paths}
import java.util.regex.Matcher
import js7.agent.data.AgentState
import js7.agent.data.orderwatch.FileWatchState
import js7.agent.scheduler.order.FileWatchManager.*
import js7.base.io.file.watch.DirectoryEvent.{FileAdded, FileDeleted}
import js7.base.io.file.watch.DirectoryEventDelayer.syntax.RichDelayLineObservable
import js7.base.io.file.watch.{DirectoryEvent, DirectoryWatch, DirectoryWatchSettings}
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.AsyncMap
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.problem.Checked
import js7.base.problem.Checked.catchNonFatal
import js7.base.thread.IOExecutor
import js7.base.time.ScalaTime.*
import js7.base.utils.LockKeeper
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.agent.AgentPath
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{KeyedEvent, Stamped}
import js7.data.item.BasicItemEvent.{ItemAttachedToMe, ItemDetached}
import js7.data.order.OrderId
import js7.data.orderwatch.FileWatch.FileArgumentName
import js7.data.orderwatch.OrderWatchEvent.{ExternalOrderArised, ExternalOrderVanished}
import js7.data.orderwatch.{ExternalOrderName, FileWatch, OrderWatchEvent, OrderWatchPath}
import js7.data.value.expression.Expression
import js7.data.value.expression.scopes.{EnvScope, NowScope}
import js7.data.value.{NamedValues, StringValue}
import js7.journal.state.Journal
import monix.eval.Task
import monix.reactive.Observable
import monix.reactive.subjects.PublishSubject
import scala.concurrent.duration.Deadline.now

/** Persists, recovers and runs FileWatches. */
final class FileWatchManager(
  ownAgentPath: AgentPath,
  journal: Journal[AgentState],
  config: Config)
  (implicit iox: IOExecutor):

  private val settings = DirectoryWatchSettings.fromConfig(config).orThrow
  private val lockKeeper = new LockKeeper[OrderWatchPath]
  private val idToStopper = AsyncMap(Map.empty[OrderWatchPath, Task[Unit]])

  def stop: Task[Unit] =
    idToStopper.removeAll
      .flatMap(_.values.toVector.parUnorderedSequence)
      .map(_.unorderedFold)

  def start: Task[Checked[Unit]] =
    journal.state
      .map(_.keyTo(FileWatchState).values)
      .flatMap(_
        .toVector
        .traverse(startWatching)
        .map(_.combineAll))

  def update(fileWatch: FileWatch): Task[Checked[Unit]] =
    lockKeeper.lock(fileWatch.path) {
      journal
        .persist(agentState =>
          Right(
            agentState.keyTo(FileWatchState).get(fileWatch.path) match {
              case Some(watchState) =>
                if watchState.fileWatch == fileWatch then
                  Nil
                else {
                  // If the directory changes, all arisen files vanish now.
                  // Beware that directory is an (EnvScope-only) Expression.
                  val vanished =
                    if watchState.fileWatch.directoryExpr == fileWatch.directoryExpr then
                      Nil
                    else
                      watchState.allFilesVanished
                  vanished.toVector :+ (NoKey <-: ItemAttachedToMe(fileWatch))
                }

              case None =>
                (NoKey <-: ItemAttachedToMe(fileWatch)) :: Nil
            }))
    }.flatMapT { case (_, agentState) =>
      startWatching(agentState.keyTo(FileWatchState)(fileWatch.path))
    }

  def remove(fileWatchPath: OrderWatchPath): Task[Checked[Unit]] =
    lockKeeper.lock(fileWatchPath):
      journal
        .persist(agentState =>
          Right(
            agentState.keyTo(FileWatchState).get(fileWatchPath) match {
              case None => Nil
              case Some(fileWatchState) =>
                // When a FileWatch is detached, all arisen files vanish now,
                // to allow proper removal of the Controller's orders (after OrderFinished), and
                // to allow the Controller to move the FileWatch to a different Agent,
                // because the other Agent will start with an empty FileWatchState.
                fileWatchState.allFilesVanished.toVector :+
                  (NoKey <-: ItemDetached(fileWatchPath, ownAgentPath))
            }))
        .flatMapT { case (_, agentState) =>
          stopWatching(fileWatchPath)
            .as(Checked.unit)
        }

  private def startWatching(fileWatchState: FileWatchState): Task[Checked[Unit]] =
    val id = fileWatchState.fileWatch.path
    logger.debugTask("startWatching", id)(Task.defer {
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
              previous.getOrElse(Task.unit) *>
                observable
                  .onCancelRaiseError(CanceledException)
                  .onErrorHandle(throwable =>
                    logger.error(throwable.toStringWithCauses))  // Ignore ???
                  .start
                  .map(fiber =>
                    // Register the stopper, a joining task for the next update:
                    logger.debugTask("stop watching", id)(Task.defer {
                      stop.onComplete()
                      fiber.join
                        .logWhenItTakesLonger(s"startWatching $id: stopping previous watcher")
                    })))
            .void)
        .logWhenItTakesLonger(s"startWatching $id")
    })

  private def stopWatching(id: OrderWatchPath): Task[Unit] =
    idToStopper
      .remove(id)
      .flatMap(_ getOrElse Task.unit)

  private def watch(fileWatchState: FileWatchState, stop: Observable[Unit]): Checked[Task[Unit]] =
    import fileWatchState.fileWatch

    fileWatch.directoryExpr
      .evalAsString(EnvScope)
      .flatMap(string =>
        catchNonFatal(Paths.get(string)))
      .map { directory =>
        val delayIterator = settings.retryDelays.iterator ++
          Iterator.continually(settings.retryDelays.last)
        var directoryState = fileWatchState.directoryState

        DirectoryWatch
          .observable(
            directory, directoryState,
            settings,
            isRelevantFile = relativePath =>
              fileWatch.resolvedPattern.matcher(relativePath.toString).matches)
          .doOnSubscribe(Task {
            logger.debug(s"${fileWatch.path} watching started - $directory")
          })
          .takeUntil(stop)
          .flatMap(Observable.fromIterable)
          // Buffers without limit all incoming events
          .delayFileAdded(directory, fileWatch.delay, settings.logDelays)
          .bufferIntrospective(1024)
          .mapEval(dirEventSeqs =>
            lockKeeper.lock(fileWatch.path)(
              emitOrderWatchEvents(fileWatch, directory, dirEventSeqs)))
          .foreachL:
            case Left(problem) => logger.error(problem.toString)
            case Right((_, agentState)) =>
              directoryState = agentState.keyTo(FileWatchState)(fileWatch.path).directoryState
          .onErrorRestartLoop(now) { (throwable, since, restart) =>
            val delay = (since + delayIterator.next()).timeLeftOrZero
            logger.error(s"Delay ${delay.pretty} after error: ${throwable.toStringWithCauses}")
            for t <- throwable.ifStackTrace do logger.debug(t.toString, t)
            Task.sleep(delay) *> restart(now)
          }
          .guaranteeCase(exitCase => Task {
            logger.debug(s"${fileWatch.path} watching $exitCase - $directory")
          })
      }

  private def emitOrderWatchEvents(
    fileWatch: FileWatch,
    directory: Path,
    dirEventSeqs: List[Seq[DirectoryEvent]])
  : Task[Checked[(Seq[Stamped[KeyedEvent[OrderWatchEvent]]], AgentState)]] =
    journal.state
      .flatMap(agentState =>
        if !agentState.keyTo(FileWatchState).contains(fileWatch.path) then
          Task.right(Nil -> agentState)
        else
          journal.persist { agentState =>
            Right(
              dirEventSeqs
                .view
                .flatten
                // In case of DirectoryWatch error recovery, duplicate DirectoryEvent may occur.
                // We check this here.
                .flatMap(dirEvent =>
                  agentState.keyTo(FileWatchState)
                    .get(fileWatch.path)
                    // Ignore late events after FileWatch has been removed
                    .flatMap(fileWatchState =>
                      dirEvent match {
                        case fileAdded @ FileAdded(path) if !fileWatchState.containsPath(path) =>
                          val maybeOrderId = pathToOrderId(fileWatch, path)
                          if maybeOrderId.isEmpty then logger.debug(s"Ignore $fileAdded (no OrderId)")
                          for orderId <- maybeOrderId yield
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
          })

  private def pathToOrderId(fileWatch: FileWatch, relativePath: Path): Option[OrderId] =
    relativePathToOrderId(fileWatch, relativePath.toString)
      .flatMap { checkedOrderId =>
        for problem <- checkedOrderId.left do logger.error(s"${fileWatch.path} $relativePath: $problem")
        checkedOrderId.toOption
      }

  private def toOrderArguments(directory: Path, path: Path) =
    NamedValues(FileArgumentName -> StringValue(directory.resolve(path).toString))


object FileWatchManager:
  private val logger = Logger[this.type]
  private val CanceledException = new Exception

  def relativePathToOrderId(fileWatch: FileWatch, relativePath: String): Option[Checked[OrderId]] =
    lazy val default = OrderId.checked(s"file:${fileWatch.path.string}:$relativePath")
    val matcher = fileWatch.resolvedPattern.matcher(relativePath)
    matcher.matches() ? {
      fileWatch.orderIdExpression match
        case None => default
        case Some(expr) =>
          evalAsString(fileWatch.path, expr, matcher)
            .flatMap(OrderId.checked)
    }

  private def evalAsString(orderWatchPath: OrderWatchPath, expression: Expression, matchedMatcher: Matcher) =
    expression.evalAsString(
      FileWatchScope(orderWatchPath, matchedMatcher) |+| NowScope() |+| EnvScope)
