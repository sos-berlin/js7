package js7.agent.motor

import cats.effect.{IO, ResourceIO}
import cats.instances.vector.*
import cats.syntax.foldable.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import com.typesafe.config.Config
import fs2.concurrent.{Signal, SignallingRef}
import java.nio.file.{Path, Paths}
import js7.agent.data.AgentState
import js7.agent.data.orderwatch.FileWatchState
import js7.agent.motor.FileWatchManager.*
import js7.base.catsutils.CatsEffectExtensions.joinStd
import js7.base.fs2utils.StreamExtensions.onStart
import js7.base.io.file.watch.DirectoryEvent.{FileAdded, FileDeleted}
import js7.base.io.file.watch.{DirectoryEvent, DirectoryEventDelayer, DirectoryWatch, DirectoryWatchSettings}
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.AsyncMap
import js7.base.problem.Checked
import js7.base.problem.Checked.catchNonFatal
import js7.base.service.Service
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.syntax.{RichF, logWhenItTakesLonger}
import js7.base.utils.Delayer.extensions.onFailureRestartWithDelayer
import js7.base.utils.LockKeeper
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.agent.AgentPath
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{EventCalc, KeyedEvent, TimeCtx}
import js7.data.item.BasicItemEvent.{ItemAttachedToMe, ItemDetached}
import js7.data.orderwatch.FileWatch.FileArgumentName
import js7.data.orderwatch.OrderWatchEvent.{ExternalOrderAppeared, ExternalOrderVanished}
import js7.data.orderwatch.{ExternalOrderName, FileWatch, OrderWatchEvent, OrderWatchPath}
import js7.data.value.expression.scopes.EnvScope
import js7.data.value.{NamedValues, StringValue}
import js7.journal.Journal
import scala.collection.View

/** Persists, recovers and runs FileWatches. */
private final class FileWatchManager private(
  ownAgentPath: AgentPath,
  journal: Journal[AgentState],
  config: Config)
extends
  Service.StoppableByRequest:

  private val settings = DirectoryWatchSettings.fromConfig(config).orThrow
  private val lockKeeper = new LockKeeper[OrderWatchPath]
  private val idToStopper = AsyncMap(Map.empty[OrderWatchPath, IO[Unit]])

  protected def start =
    journal.aggregate
      .map(_.keyTo(FileWatchState).values)
      .flatMap(_
        .toVector
        .traverse(startWatching)
        .map(_.combineAll))
      .map(_.orThrow)
      .requireElementType[Unit]
      .productR:
        startService:
          untilStopRequested *> stopMe

  private def stopMe: IO[Unit] =
    idToStopper.removeAll
      .flatMap(_.values.toVector.parUnorderedSequence)
      .map(_.unorderedFold)

  def update(fileWatch: FileWatch): IO[Checked[Unit]] =
    lockKeeper.lock(fileWatch.path) {
      journal.persist: agentState =>
        agentState.keyTo(FileWatchState).get(fileWatch.path) match
          case Some(watchState) =>
            if watchState.fileWatch == fileWatch then
              Nil
            else
              // If the directory changes, all appeared files vanish now.
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
      .flatMapT: persisted =>
        startWatching(persisted.aggregate.keyTo(FileWatchState)(fileWatch.path))
    }

  def remove(fileWatchPath: OrderWatchPath): IO[Checked[Unit]] =
    lockKeeper.lock(fileWatchPath) {
      journal.persist: agentState =>
        agentState.keyTo(FileWatchState).get(fileWatchPath).foldMap: fileWatchState =>
          // When a FileWatch is detached, all appeared files vanish now,
          // to allow proper removal of the Controller's orders (after OrderFinished), and
          // to allow the Controller to move the FileWatch to a different Agent,
          // because the other Agent will start with an empty FileWatchState.
          fileWatchState.allFilesVanished.toVector :+
            (NoKey <-: ItemDetached(fileWatchPath, ownAgentPath))
      .flatMapT: _ =>
        stopWatching(fileWatchPath)
          .as(Checked.unit)
    }

  private def startWatching(fileWatchState: FileWatchState): IO[Checked[Unit]] =
    val id = fileWatchState.fileWatch.path
    import fileWatchState.fileWatch
    logger.debugIO(s"${fileWatchState.fileWatch.path}: startWatching", id):
      SignallingRef[IO, Boolean](false).flatMap: stop =>
        watch(fileWatchState, stop).traverse: watch =>
          // Execute previously registered stopper (which awaits completion),
          // and start our stream as a fiber.
          // At the same time, register a stopper in idToStopper.
          // The stopper is a task that stops the stream and awaits its completion.
          idToStopper.update(fileWatchState.id): previous =>
            // Wait for previous Stream to complete (stop and fiber.join)
            previous.getOrElse(IO.unit) *>
              watch
                .handleError: throwable =>
                  logger.error(s"${fileWatch.path}: ${throwable.toStringWithCauses}") // Ignore ???
                .start
                .map: fiber =>
                  // Register the stopper, a joining task for the next update:
                  logger.debugIO(s"${fileWatch.path}: Stop watching", id):
                    stop.set(true) *>
                      fiber.joinStd
                        .logWhenItTakesLonger(s"$id: stopping previous watcher")
          .void
        .logWhenItTakesLonger(s"startWatching $id")

  private def stopWatching(id: OrderWatchPath): IO[Unit] =
    logger.debugIO(s"$id: stopWatching"):
      idToStopper.remove(id)
        .flatMap(_ getOrElse IO.unit)

  private def watch(fileWatchState: FileWatchState, stop: Signal[IO, Boolean]): Checked[IO[Unit]] =
    import fileWatchState.fileWatch
    fileWatch.directoryExpr
      .evalAsString(using EnvScope)
      .flatMap: string =>
        catchNonFatal(Paths.get(string))
      .map: directory =>
        watchDirectory(fileWatchState, stop, directory)

  private def watchDirectory(
    fileWatchState: FileWatchState, stop: Signal[IO, Boolean], directory: Path)
  : IO[Unit] =
    import fileWatchState.fileWatch
    logger.debugIO(s"${fileWatch.path}: watchDirectory", directory):
      IO.defer:
        var directoryState = fileWatchState.directoryState
        DirectoryWatch
          .stream(
            directory, directoryState, settings,
            isRelevantFile = relativePath => fileWatch.matchesFilename(relativePath.toString))
          .onStart:
            IO(logger.debug(s"${fileWatch.path} watching started: $directory"))
          // Buffers all incoming events, unlimited
          .interruptWhen[IO](stop)
          .through:
            DirectoryEventDelayer(directory, fileWatch.delay, settings.logDelayConf)
          .chunks
          .evalMap: chunk =>
            lockKeeper.lock(fileWatch.path):
              emitOrderWatchEvents(fileWatch, directory, chunk)
          .foreach:
            case Left(problem) => IO(logger.error(s"${fileWatch.path}: $problem"))
            case Right(persisted) => IO:
              directoryState =
                persisted.aggregate.keyTo(FileWatchState)(fileWatch.path).directoryState
          .compile.drain
          .onFailureRestartWithDelayer(settings.delayConf): (delayer, throwable) =>
            delayer.peekNextDelay.map: delay =>
              logger.error(s"${fileWatch.path}: Delay ${delay.pretty} after error: ${
                throwable.toStringWithCauses}")
              for t <- throwable.ifStackTrace do logger.debug(s"${fileWatch.path}: $t", t)

  private def orderWatchEvents(
    fileWatch: FileWatch,
    directory: Path,
    dirEventSeqs: Seq[DirectoryEvent])
  : EventCalc[AgentState, OrderWatchEvent, TimeCtx] =
    EventCalc.multiple: agentState =>
      agentState.keyTo(FileWatchState).get(fileWatch.path)
        // Ignore late events after FileWatch has been removed
        .toVector
        .flatMap: fileWatchState =>
          dirEventSeqs.flatMap:
            // In case of DirectoryWatch error recovery, duplicate DirectoryEvent may occur.
            // We check this here.
            case FileAdded(path)
              if !fileWatchState.containsPath(path) =>
              Some(ExternalOrderAppeared(
                ExternalOrderName.unchecked(path.toString),
                toOrderArguments(directory, path)))

            case FileDeleted(path) if fileWatchState.containsPath(path) =>
              Some(ExternalOrderVanished(ExternalOrderName.unchecked(path.toString)))

            case event =>
              logger.debug(s"${fileWatch.path} 🪱 Ignore $event")
              None
        .map(fileWatch.path <-: _)

  private def toOrderArguments(directory: Path, path: Path) =
    NamedValues(FileArgumentName -> StringValue(directory.resolve(path).toString))


private object FileWatchManager:
  private val logger = Logger[this.type]

  def resource(
    ownAgentPath: AgentPath,
    journal: Journal[AgentState],
    config: Config)
  : ResourceIO[FileWatchManager] =
    Service.resource:
      new FileWatchManager(ownAgentPath, journal, config)
