package js7.base.log.reader

import cats.effect.{IO, Resource, ResourceIO}
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import com.typesafe.config.Config
import fs2.concurrent.Channel
import fs2.{Chunk, Stream}
import java.nio.file.Path
import java.nio.file.StandardWatchEventKinds.{ENTRY_CREATE, ENTRY_DELETE}
import java.time.ZoneId
import java.util.NoSuchElementException
import js7.base.bean.MBeanUtils.registerStaticMBean
import js7.base.catsutils.CatsEffectExtensions.defer
import js7.base.catsutils.UnsafeMemoizable
import js7.base.catsutils.UnsafeMemoizable.memoize
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.file.watch.{DirectoryEvent, DirectoryState, DirectoryWatch}
import js7.base.log.Logger.syntax.*
import js7.base.log.reader.LogDirectoryIndex.*
import js7.base.log.reader.LogIndex.LogDirectoryIndexMXBean
import js7.base.log.reader.LogUtils.{deleteTmpFiles, isOurLogFilename}
import js7.base.log.{LogLevel, Logger}
import js7.base.service.Service
import js7.base.time.ScalaTime.*
import js7.base.utils.Allocated
import js7.base.utils.CatsUtils.syntax.*
import js7.base.utils.ScalaUtils.syntax.*

/** Provides a LogIndex for each pair of logFilePrefix and LogLevel (Error, Info and Debug).
  *
  * LogDirectoryIndex is not itself an index but provides LogIndexes.
  *
  * Directory watching is started when the first LogIndex is provided.
  */
final class LogDirectoryIndex private(directory: Path, logFilePrefixes: Set[String])
  (using zoneId: ZoneId, conf: LogIndexConf)
extends Service.StoppableByRequest:

  private val lazyPrefixAndLevelToIndex
  : IO[Allocated[IO, Map[(String, LogLevel), Allocated[IO, LogIndex]]]] =
    memoize:
      watching.toAllocated

  protected def start =
    startService:
      deleteTmpFiles(directory, logFilePrefixes) *>
        untilStopRequested.guarantee:
          release

  private def release =
    // When `watching` has not been called yet, it will be called now while isStopping = true,
    // and return an empty Map.
    lazyPrefixAndLevelToIndex.flatMap:
      _.release /*stop watching*/

  def logIndex(logFilePrefix: String, logLevel: LogLevel): IO[LogIndex] =
    lazyPrefixAndLevelToIndex.flatMap: levelToIndex =>
      levelToIndex.allocatedThing.get((logFilePrefix, logLevel)) match
        case None =>
          if !logFilePrefixes(logFilePrefix) then
            IO.raiseError(NoSuchElementException(s"Unknown logLinePrefix=$logFilePrefix"))
          else if !LogUtils.LogLevels(logLevel) then
            IO.raiseError(NoSuchElementException(s"Unsupported $logLevel LogLevel"))
          else
            IO.raiseError(NoSuchElementException(s"$this has been stopped"))
        case Some(index) =>
          IO.pure(index.allocatedThing)

  /** Run a LogIndex for each LogLevel. */
  private def watching: ResourceIO[Map[(String, LogLevel), Allocated[IO, LogIndex]]] =
    Resource.defer:
      if isStopping then
        Resource.pure(Map.empty)
      else
        logger.traceResource:
          for
            prefixAndLevelToFilesAndChannel <- watchDirectoryAndDispatchEvents
            levelToIndex <-
              Resource.make(
                acquire =
                  IO.parSequence:
                    for
                      logFilePrefix <- logFilePrefixes.toSeq
                      logLevel <- LogUtils.LogLevels
                    yield
                      val (initialFiles, channel) =
                        prefixAndLevelToFilesAndChannel((logFilePrefix, logLevel))
                      LogIndex.directory(
                        directory,
                        initialFiles,
                        channel.stream.unchunks,
                        watchGrowth = true,
                        label = s"$logFilePrefix/$logLevel"
                      ).toAllocated.map: allocated =>
                        (logFilePrefix, logLevel) -> allocated
                  .map(_.toMap))(
                release =
                  _.values.toSeq.parTraverseVoid:
                    _.release /*LogIndex*/)
          yield
            levelToIndex

  /** Return for each LogLevel the initial files and a Channel of DirectoryEvents. */
  private def watchDirectoryAndDispatchEvents
  : Resource[IO, Map[(String, LogLevel), (Seq[Path], Channel[IO, Chunk[DirectoryEvent]])]] =
    logger.traceResource:
      for
        prefixAndLevelToChannel <- makePrefixAndLevelToChannel
        (initialFiles, directoryEvents) <- Resource.eval:
          watchDirectory(directory, isOurLogFilename(logFilePrefixes, _))
        result <-
          directoryEvents.chunks.evalMap: events =>
            events.asSeq.groupBy: event =>
              LogUtils.fileToPrefixAndLogLevel(event.relativePath)
            .toSeq.traverse:
              case (None, events) => IO:
                for event <- events do logger.trace(s"Does not match: $event")
              case (Some((prefix, logLevel)), events) =>
                prefixAndLevelToChannel((prefix, logLevel))
                  .send(Chunk.from(events))
          .compile.drain
          .background
          .as:
            prefixAndLevelToChannel.view.map: (prefixAndLevel, channel) =>
              val ourInitialFiles = initialFiles.filter:
                LogUtils.fileToPrefixAndLogLevel(_).contains(prefixAndLevel)
              prefixAndLevel -> (ourInitialFiles, channel)
            .toMap
      yield result

  private def makePrefixAndLevelToChannel
  : ResourceIO[Map[(String, LogLevel), Channel[IO, Chunk[DirectoryEvent]]]] =
    // We use Channel instead of a Queue, because Queue#offer(None) would block when
    // the reader has terminated while a Channel can be closed anytime without blocking.
    Resource.make(
      acquire =
        locally:
          for
            prefix <- logFilePrefixes.toSeq
            logLevel <- LogUtils.LogLevels
          yield
            Channel.bounded[IO, Chunk[DirectoryEvent]](1).map: channel =>
              (prefix, logLevel) -> channel
        .sequence.map(_.toMap))(
      release =
        _.values.foldMap: channel =>
          channel.close.void)

  override def toString = "LogDirectoryIndex"


object LogDirectoryIndex:
  private val logger = Logger[this.type]

  def resource(directory: Path)(using zoneId: ZoneId, config: Config)
  : ResourceIO[LogDirectoryIndex] =
    resource(directory, Set(config.getString("js7.log.prefix")))

  def resource(directory: Path, logFilePrefixes: Set[String])(using zoneId: ZoneId, config: Config)
  : ResourceIO[LogDirectoryIndex] =
    given LogIndexConf =
      LogIndexConf.fromConfig(config).orThrow
    for
      _ <- registerStaticMBean[LogDirectoryIndexMXBean]("LogDirectoryIndex", LogIndex.Bean)
      service <-
        Service:
          LogDirectoryIndex(directory, logFilePrefixes)
    yield
      service

  private def watchDirectory(directory: Path, isRelevantFile: Path => Boolean)
    (using conf: LogIndexConf)
  : IO[(Vector[Path], Stream[IO, DirectoryEvent])] =
    directory.directoryStream[IO]
      .filter:
        isRelevantFile
      .compile.toVector.map: files =>
        files ->
          DirectoryWatch.stream(
            directory,
            DirectoryState(files.map(_.filename)),
            conf.directoryWatchSettings.copy(watchDelay = 0.s),
            isRelevantFile,
            Set(ENTRY_CREATE, ENTRY_DELETE))
