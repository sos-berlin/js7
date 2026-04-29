package js7.base.log.reader

import cats.effect.std.Queue
import cats.effect.{IO, Resource, ResourceIO}
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import com.typesafe.config.Config
import fs2.{Chunk, Stream}
import java.nio.file.Path
import java.time.ZoneId
import js7.base.catsutils.CatsEffectExtensions.defer
import js7.base.catsutils.UnsafeMemoizable
import js7.base.catsutils.UnsafeMemoizable.memoize
import js7.base.io.file.watch.DirectoryEvent
import js7.base.log.Logger.syntax.*
import js7.base.log.reader.LogDirectoryIndexRegister.*
import js7.base.log.{LogLevel, Logger}
import js7.base.service.Service
import js7.base.system.MBeanUtils.registerStaticMBean
import js7.base.utils.Allocated
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ScalaUtils.syntax.*

/** Contains LogDirectoryIndexes for Error, Info and Debug.
  *
  * LogDirectoryIndexes including directory watching are started only when used.
  */
final class LogDirectoryIndexRegister private(directory: Path)(using zoneId: ZoneId, config: Config)
extends Service.StoppableByRequest:

  private val lazyLevelToIndex: IO[Allocated[IO, Map[LogLevel, Allocated[IO, LogDirectoryIndex]]]] =
    memoize:
      watching.toAllocated

  protected def start =
    startService:
      untilStopRequested *> release

  protected def release =
    // When startWatching has not been called yet, it will be called now with isStopping = true,
    // and return an empty Map.
    lazyLevelToIndex.flatMap:
      _.release

  def forLogLevel(logLevel: LogLevel): IO[LogDirectoryIndex] =
    lazyLevelToIndex.flatMap: levelToIndex =>
      levelToIndex.allocatedThing.get(logLevel) match
        case None =>
          IO.raiseError(new IllegalArgumentException(
            s"Unsupported logLevel=$logLevel or $this has been stopped"))

        case Some(index) =>
          IO.pure(index.allocatedThing)

  /** Run a LogDirectoryIndex for each LogLevel. */
  private def watching: ResourceIO[Map[LogLevel, Allocated[IO, LogDirectoryIndex]]] =
    Resource.defer:
      if isStopping then
        Resource.pure(Map.empty)
      else
        logger.debugResource:
          for
            levelToFilesAndQueue <- watchDirectoryAndDispatchEvents
            levelToIndex <-
              Resource.make(
                acquire =
                  LogDirectoryIndex.LogLevels.toSeq.traverse: logLevel =>
                    val (files, queue) = levelToFilesAndQueue(logLevel)
                    LogDirectoryIndex.directory(
                      directory, logLevel, files,
                      fs2.Stream.fromQueueNoneTerminatedChunk(queue)
                    ).toAllocated
                      .map(logLevel -> _)
                  .map(_.toMap))(
                release =
                  _.values.toSeq.parTraverseVoid(_.release))
          yield
            levelToIndex

  /** Return for each LogLevel the initial files and a Queue of DirectoryEvents. */
  private def watchDirectoryAndDispatchEvents
  : Resource[IO, Map[LogLevel, (Seq[Path], Queue[IO, Option[Chunk[DirectoryEvent]]])]] =
    for
      levelToQueue <- Resource.eval(makeLevelToQueue)
      // TODO Erst lesen, wenn LogDirectoryIndexRegister gebraucht wird ?
      (files, directoryEvents) <-
        Resource.eval(watchDirectory)
      _ <-
        directoryEvents.filter: o =>
          !LogDirectoryIndex.isOurTmpFile(o.relativePath) // to be sure
        .chunks.evalMap: events =>
          events.asSeq.groupBy: event =>
            LogDirectoryIndex.fileToLogLevel(event.relativePath)
          .toSeq.traverse: (logLevel, events) =>
            levelToQueue(logLevel)
              .offer(Some(Chunk.from(events)))
        .compile.drain
        .guarantee:
          levelToQueue.values.toSeq.foldMap: queue =>
            queue.offer(None)
        .background
    yield
      levelToQueue.view.map: (logLevel, queue) =>
        logLevel -> (files.filter(LogDirectoryIndex.fileToLogLevel(_) ==  logLevel), queue)
      .toMap

  /** Return the initial files and a Stream of DirectoryEvents. */
  private def watchDirectory: IO[(Vector[Path], Stream[IO, DirectoryEvent])] =
    val isCurrentLog = Set(
      config.getString("js7.log.error.file"),
      config.getString("js7.log.info.file"),
      config.getString("js7.log.debug.file"))

    def isValidFile(file: Path) =
      val filename = file.getFileName
      val name = filename.toString
      isCurrentLog(name) ||
        !name.startsWith("~") &&
          isCompressedLogFile(file) &&
          !LogDirectoryIndex.isOurTmpFile(file) // to be sure

    LogDirectoryIndex.watchDirectory(directory, isValidFile)


  override def toString = "LogDirectoryIndexRegister"


object LogDirectoryIndexRegister:
  private val logger = Logger[this.type]
  private given ZoneId = ZoneId.systemDefault

  def resource(directory: Path)(using config: Config): ResourceIO[LogDirectoryIndexRegister] =
    for
      _ <- registerStaticMBean[LogDirectoryIndex.LogDirectoryIndexMXBean]("LogDirectoryIndex", LogDirectoryIndex.Bean).void
      service <-
        Service:
          new LogDirectoryIndexRegister(directory)
    yield
      service

  private def makeLevelToQueue: IO[Map[LogLevel, Queue[IO, Option[Chunk[DirectoryEvent]]]]] =
    LogDirectoryIndex.LogLevels.toSeq.traverse: logLevel =>
      Queue.bounded[IO, Option[Chunk[DirectoryEvent]]](1).map: queue =>
        logLevel -> queue
    .map(_.toMap)

  private def isCompressedLogFile(file: Path): Boolean =
    file.toString.endsWith(".log.gz")
