package js7.agent.scheduler.order

import cats.effect.IO
import cats.instances.vector.*
import cats.syntax.foldable.*
import cats.syntax.monoid.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import com.typesafe.config.Config
import fs2.Chunk
import fs2.concurrent.{Signal, SignallingRef}
import java.nio.file.{Path, Paths}
import java.util.regex.Matcher
import js7.agent.data.AgentState
import js7.agent.data.orderwatch.FileWatchState
import js7.agent.scheduler.order.FileWatchManager.*
import js7.base.catsutils.CatsEffectExtensions.{joinStd, right}
import js7.base.fs2utils.StreamExtensions.onStart
import js7.base.io.file.watch.DirectoryEvent.{FileAdded, FileDeleted}
import js7.base.io.file.watch.{DirectoryEvent, DirectoryEventDelayer, DirectoryWatch, DirectoryWatchSettings}
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixlike.MonixLikeExtensions.*
import js7.base.monixutils.AsyncMap
import js7.base.problem.Checked
import js7.base.problem.Checked.catchNonFatal
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
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
import scala.collection.View
import scala.concurrent.duration.Deadline.now

/** Persists, recovers and runs FileWatches. */
final class FileWatchManager(
  ownAgentPath: AgentPath,
  journal: Journal[AgentState],
  config: Config):

  private val settings = DirectoryWatchSettings.fromConfig(config).orThrow
  private val lockKeeper = new LockKeeper[OrderWatchPath]
  private val idToStopper = AsyncMap(Map.empty[OrderWatchPath, IO[Unit]])

  def stop: IO[Unit] =
    idToStopper.removeAll
      .flatMap(_.values.toVector.parUnorderedSequence)
      .map(_.unorderedFold)

  def start: IO[Checked[Unit]] =
    journal.state
      .map(_.keyTo(FileWatchState).values)
      .flatMap(_
        .toVector
        .traverse(startWatching)
        .map(_.combineAll))

  def update(fileWatch: FileWatch): IO[Checked[Unit]] =
    lockKeeper.lock(fileWatch.path):
      journal.persist: agentState =>
        Right:
          agentState.keyTo(FileWatchState).get(fileWatch.path) match
            case Some(watchState) =>
              if watchState.fileWatch == fileWatch then
                Nil
              else
                // If the directory changes, all arisen files vanish now.
                // Note that directory is an (EnvScope-only) Expression.
                val vanished =
                  def reduce(fw: FileWatch) = fw.copy(
                    agentPath = AgentPath.empty,
                    pattern = None,
                    delay = 0.s,
                    itemRevision = None)
                  if reduce(fileWatch) == reduce(watchState.fileWatch) then
                    View.empty
                  else
                    watchState.allFilesVanished
                (vanished :+ (NoKey <-: ItemAttachedToMe(fileWatch)))
                  .toVector

            case None =>
              (NoKey <-: ItemAttachedToMe(fileWatch)) :: Nil
    .flatMapT: (_, agentState) =>
      startWatching(agentState.keyTo(FileWatchState)(fileWatch.path))

  def remove(fileWatchPath: OrderWatchPath): IO[Checked[Unit]] =
    lockKeeper.lock(fileWatchPath):
      journal
        .persist: agentState =>
          Right:
            agentState.keyTo(FileWatchState).get(fileWatchPath) match
              case None => Nil
              case Some(fileWatchState) =>
                // When a FileWatch is detached, all arisen files vanish now,
                // to allow proper removal of the Controller's orders (after OrderFinished), and
                // to allow the Controller to move the FileWatch to a different Agent,
                // because the other Agent will start with an empty FileWatchState.
                fileWatchState.allFilesVanished.toVector :+
                  (NoKey <-: ItemDetached(fileWatchPath, ownAgentPath))
        .flatMapT: (_, agentState) =>
          stopWatching(fileWatchPath)
            .as(Checked.unit)

  private def startWatching(fileWatchState: FileWatchState): IO[Checked[Unit]] =
    val id = fileWatchState.fileWatch.path
    logger.debugIO("startWatching", id):
      SignallingRef[IO, Boolean](false).flatMap: stop =>
        watch(fileWatchState, stop)
          .traverse: stream =>
            // Execute previously registered stopper (which awaits completion),
            // and start our stream as a fiber.
            // At the same time, register a stopper in idToStopper.
            // The stopper is a task that stops the stream and awaits its completion.
            idToStopper
              .update(fileWatchState.id, previous =>
                // Wait for previous Stream to complete (stop and fiber.join)
                previous.getOrElse(IO.unit) *>
                  stream
                    //?.onCancelRaiseError(CanceledException)
                    .handleError: throwable =>
                      logger.error(throwable.toStringWithCauses)  // Ignore ???
                    .start
                    .map: fiber =>
                      // Register the stopper, a joining task for the next update:
                      logger.debugIO("stop watching", id):
                        stop.set(true) *>
                          fiber.joinStd
                            .logWhenItTakesLonger(s"startWatching $id: stopping previous watcher"))
              .void
          .logWhenItTakesLonger(s"startWatching $id")

  private def stopWatching(id: OrderWatchPath): IO[Unit] =
    idToStopper
      .remove(id)
      .flatMap(_ getOrElse IO.unit)

  private def watch(fileWatchState: FileWatchState, stop: Signal[IO, Boolean]): Checked[IO[Unit]] =
    import fileWatchState.fileWatch

    fileWatch.directoryExpr
      .evalAsString(EnvScope)
      .flatMap(string =>
        catchNonFatal(Paths.get(string)))
      .map: directory =>
        val delayIterator = settings.retryDelays.iterator ++
          Iterator.continually(settings.retryDelays.last)
        var directoryState = fileWatchState.directoryState

        DirectoryWatch
          .stream(
            directory, directoryState,
            settings,
            isRelevantFile = relativePath =>
              fileWatch.resolvedPattern.matcher(relativePath.toString).matches)
          .onStart(IO:
            logger.debug(s"${fileWatch.path} watching started - $directory"))
          .interruptWhen(stop)
          // Buffers without limit all incoming events
          .through:
            DirectoryEventDelayer(directory, fileWatch.delay, settings.logDelays)
          .chunks
          .evalMap: chunk =>
            lockKeeper.lock(fileWatch.path):
              emitOrderWatchEvents(fileWatch, directory, chunk)
          .foreach:
            case Left(problem) => IO(logger.error(problem.toString))
            case Right((_, agentState)) => IO:
              directoryState = agentState.keyTo(FileWatchState)(fileWatch.path).directoryState
          .compile.drain
          .onErrorRestartLoop(now): (throwable, since, restart) =>
            val delay = (since + delayIterator.next()).timeLeftOrZero
            logger.error(s"Delay ${delay.pretty} after error: ${throwable.toStringWithCauses}")
            for t <- throwable.ifStackTrace do logger.debug(t.toString, t)
            IO.sleep(delay) *> restart(now)
          .guaranteeCase(exitCase => IO:
            logger.debug(s"${fileWatch.path} watching $exitCase - $directory"))

  private def emitOrderWatchEvents(
    fileWatch: FileWatch,
    directory: Path,
    dirEventSeqs: Chunk[DirectoryEvent])
  : IO[Checked[(Seq[Stamped[KeyedEvent[OrderWatchEvent]]], AgentState)]] =
    journal.state.flatMap: agentState =>
      if !agentState.keyTo(FileWatchState).contains(fileWatch.path) then
        IO.right(Nil -> agentState)
      else
        journal.persist: agentState =>
          Right:
            agentState.keyTo(FileWatchState)
              .get(fileWatch.path)
              // Ignore late events after FileWatch has been removed
              .toVector
              .flatMap: fileWatchState =>
                dirEventSeqs
                  // In case of DirectoryWatch error recovery, duplicate DirectoryEvent may occur.
                  // We check this here.
                  .asSeq
                  .flatMap: dirEvent =>
                    dirEvent match
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
              .map(fileWatch.path <-: _)

  private def pathToOrderId(fileWatch: FileWatch, relativePath: Path): Option[OrderId] =
    relativePathToOrderId(fileWatch, relativePath.toString)
      .flatMap: checkedOrderId =>
        for problem <- checkedOrderId.left do logger.error(s"${fileWatch.path} $relativePath: $problem")
        checkedOrderId.toOption

  private def toOrderArguments(directory: Path, path: Path) =
    NamedValues(FileArgumentName -> StringValue(directory.resolve(path).toString))


object FileWatchManager:
  private val logger = Logger[this.type]
  //private val CanceledException = new Exception

  def relativePathToOrderId(fileWatch: FileWatch, relativePath: String): Option[Checked[OrderId]] =
    lazy val default = OrderId.checked(s"file:${fileWatch.path.string}:$relativePath")
    val matcher = fileWatch.resolvedPattern.matcher(relativePath)
    matcher.matches() ?
      fileWatch.orderIdExpression.match
        case None => default
        case Some(expr) =>
          evalAsString(fileWatch.path, expr, matcher)
            .flatMap(OrderId.checked)

  private def evalAsString(orderWatchPath: OrderWatchPath, expression: Expression, matchedMatcher: Matcher) =
    expression.evalAsString:
      FileWatchScope(orderWatchPath, matchedMatcher) |+| NowScope() |+| EnvScope
