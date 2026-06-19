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
import js7.base.log.reader.LogDirectoryIndex.LogDirectoryIndexMXBean
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
extends Service.TrivialReleasable:

  // TODO Make lazy for each LogLevel separately?
  private val lazyLevelToIndex: IO[Allocated[IO, Map[LogLevel, Allocated[IO, LogDirectoryIndex]]]] =
    memoize:
      watching.toAllocated

  private val logFilePrefix = config.getString("js7.log.prefix")
  private val currentErrorLogFilePrefix = logFilePrefix + "-error."
  private val currentInfoLogFilePrefix = logFilePrefix + "."
  private val currentDebugLogFilePrefix = logFilePrefix + "-debug."
  private val compressedErrorLogFilePrefix = logFilePrefix + "-error-"
  private val compressedInfoLogFilePrefix = logFilePrefix + "-"
  private val compressedDebugLogFilePrefix = logFilePrefix + "-debug-"

  protected def release =
    // When startWatching has not been called yet, it will be called now with isStopping = true,
    // and return an empty Map.
    lazyLevelToIndex.flatMap: allo =>
      allo.allocatedThing.values.foldMap:
        _.release
      *> allo.release

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
        for
          levelToFilesAndQueue <- watchDirectoryAndDispatchEvents
          levelToIndex <-
            Resource.make(
              acquire =
                LogDirectoryIndex.LogLevels.toSeq.parTraverse: logLevel =>
                  val (files, queue) = levelToFilesAndQueue(logLevel)
                  LogDirectoryIndex.directory(
                      directory, logLevel, files,
                      Stream.fromQueueNoneTerminatedChunk(queue),
                      watchGrowth = true
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
      (files, directoryEvents) <- Resource.eval(watchDirectory)
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
    def isCurrentFile(filename: String) =
      filename.endsWith(".log") && (
        filename.startsWith(currentErrorLogFilePrefix) ||
        filename.startsWith(currentInfoLogFilePrefix) ||
        filename.startsWith(currentDebugLogFilePrefix))

    def isCompressedFile(filename: String): Boolean =
      filename.endsWith(".log.gz") && (
        filename.startsWith(compressedErrorLogFilePrefix) ||
        filename.startsWith(compressedInfoLogFilePrefix) ||
        filename.startsWith(compressedDebugLogFilePrefix))

    def isRelevantFile(file: Path) =
      val name = file.getFileName.toString
      isCurrentFile(name) || isCompressedFile(name)

    LogDirectoryIndex.watchDirectory(directory, isRelevantFile)


  override def toString = "LogDirectoryIndexRegister"


object LogDirectoryIndexRegister:
  private val logger = Logger[this.type]
  private given ZoneId = ZoneId.systemDefault

  def resource(directory: Path)(using config: Config): ResourceIO[LogDirectoryIndexRegister] =
    for
      _ <- registerStaticMBean[LogDirectoryIndexMXBean]("LogDirectoryIndex", LogDirectoryIndex.Bean)
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
