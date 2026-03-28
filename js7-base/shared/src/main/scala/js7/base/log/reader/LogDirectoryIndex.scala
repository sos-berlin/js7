package js7.base.log.reader

import cats.effect.std.AtomicCell
import cats.effect.{IO, Resource, ResourceIO, SyncIO}
import cats.syntax.option.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import com.typesafe.config.Config
import fs2.Stream
import java.io.{BufferedInputStream, FileInputStream, InputStream}
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.nio.file.StandardWatchEventKinds.{ENTRY_CREATE, ENTRY_DELETE}
import java.nio.file.{Files, Path, Paths}
import java.time.{Instant, ZoneId}
import java.util.concurrent.ConcurrentSkipListMap
import java.util.zip.GZIPInputStream
import js7.base.catsutils.CatsEffectExtensions.run
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.convert.AsJava.StringAsPath
import js7.base.data.ByteArray
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.file.watch.{DirectoryEvent, DirectoryState, DirectoryWatch, DirectoryWatchSettings}
import js7.base.io.file.{ByteSeqFileReader, FileDeleter}
import js7.base.log.LogLevel.{Debug, Info}
import js7.base.log.Logger.syntax.*
import js7.base.log.reader.LogDirectoryIndex.*
import js7.base.log.reader.LogFileReader.UniqueHeaderSize
import js7.base.log.reader.{LogDirectoryIndex, LogFileIndex, LogLineKey}
import js7.base.log.{LogLevel, Logger}
import js7.base.service.Service
import js7.base.time.EpochNano
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.bytesPerSecondString
import js7.base.utils.Assertions.assertThat
import js7.base.utils.AutoClosing.syntax.use
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Allocated, ConcurrentHashMap}
import scala.concurrent.duration.{Deadline, FiniteDuration}
import scala.jdk.CollectionConverters.*

/** Provides a continuous stream of log lines over all log files in the directory.
  * @param directoryEvents updates the file list, must emit events only from `logDirectory`
  * @param pollDuration when growing log files should be respected (uncompressed only)
  */
final class LogDirectoryIndex private(
  initialLogFiles: Iterable[LogFile],
  directoryEvents: Stream[IO, OurDirEvent],
  logLevel: LogLevel,
  pollDuration: Option[FiniteDuration])
  (using zoneId: ZoneId)
extends Service.StoppableByRequest:

  private val instantToLogFile =
    new ConcurrentSkipListMap(initialLogFiles.toKeyedMap(_.fileInstant).asJava)
  private val fileToInstant: ConcurrentHashMap[Path, Instant] =
    ConcurrentHashMap.from:
      instantToLogFile.asScala.toMap.map((instant, logFile) => logFile.filename -> instant)

  import ByteSeqFileReader.BufferSize as DefaultBufferSize

  protected def start =
    startService:
      directoryEvents.chunks.map: chunk =>
        chunk.asSeq.foreachWithBracket()((evt, br) => logger.debug(s"$br$evt"))
        chunk
      .unchunks
      .evalMap:
        case FileAdded(file) =>
          IO.uncancelable: _ =>
            toLogFile(file).map: maybe =>
              maybe.foreach: logFile =>
                fileToInstant.put(logFile.filename, logFile.fileInstant)
                instantToLogFile.put(logFile.fileInstant, logFile)

        case FileDeleted(file) =>
          IO.uncancelable: _ =>
            fileToInstant.remove(file).foldMap: instant =>
              Option(instantToLogFile.remove(instant)).foldMap: logFile =>
                logger.debug(s"Remove $logFile")
                logFile.release
      .compile.drain
      .race:
        untilStopRequested
      .productR:
        IO.defer:
          instantToLogFile.values.asScala.toVector.parFoldMapA: logFile =>
            logFile.release

  def instantToLogLineKey(instant: Instant): IO[Option[LogLineKey]] =
    instantToKeyedByteLogLineStream(instant)
      .head.compile.last.map(_.map(_.key))

  def instantToKeyedByteLogLineStream(begin: Instant, byteChunkSize: Int = DefaultBufferSize)
  : Stream[IO, KeyedByteLogLine] =
    Stream.suspend:
      if instantToLogFile.isEmpty then
        Stream.empty
      else
        val logFile = instantToLogFile(begin)
        streamAndContinueWithNextFile(logFile, begin, byteChunkSize = byteChunkSize)

  def keyToKeyedByteLogLineStream(logLineKey: LogLineKey, byteChunkSize: Int = DefaultBufferSize)
  : Stream[IO, KeyedByteLogLine] =
    Stream.suspend:
      if logLineKey.logLevel != logLevel then throw IllegalArgumentException(s"Wrong LogLevel")
      if instantToLogFile.isEmpty then
        Stream.empty
      else
        val logFile = instantToLogFile(logLineKey.fileInstant)
        streamAndContinueWithNextFile(logFile, logLineKey.position, byteChunkSize = byteChunkSize)

  private def instantToLogFile(instant: Instant): LogFile =
    instantToLogFile.floorEntry(instant) match
      case null => instantToLogFile.firstEntry.getValue
      case o => o.getValue

  private def streamAndContinueWithNextFile(logFile: LogFile, instant: Instant, byteChunkSize: Int)
  : Stream[IO, KeyedByteLogLine] =
    Stream.eval:
      toDeferredIndex(logFile)
    .map:
      _.logFileIndex
    .flatMap: logFileIndex =>
      logFileIndex.streamPosAndLine(instant, byteChunkSize = byteChunkSize).map: (pos, line) =>
        KeyedByteLogLine(logFile.toLogLineKey(logLevel, pos), line)
    .append:
      streamSectionAfter(logFile.fileInstant, byteChunkSize = byteChunkSize)

  private def streamAndContinueWithNextFile(logFile: LogFile, position: Long, byteChunkSize: Int)
  : Stream[IO, KeyedByteLogLine] =
    Stream.eval:
      toDeferredIndex(logFile) // TODO DON'T BUILD logFile.logFileIndex. Only decompressed file is required
    .flatMap: deferredIndex =>
      Stream.resource:
        ByteSeqFileReader.resource[fs2.Chunk[Byte]](deferredIndex.file, bufferSize = byteChunkSize)
    .flatMap: reader =>
      reader.streamPosAndLines(position).map: (pos, line) =>
        KeyedByteLogLine(logFile.toLogLineKey(logLevel, pos), line)
    .append:
      streamSectionAfter(logFile.fileInstant, byteChunkSize = byteChunkSize)

  private def streamSectionAfter(instant: Instant, byteChunkSize: Int)
  : Stream[IO, KeyedByteLogLine] =
    Stream.suspend:
      Option:
        instantToLogFile.higherEntry(instant)
      .map(_.getValue).fold(Stream.empty): logFile =>
        streamAndContinueWithNextFile(logFile, instant, byteChunkSize = byteChunkSize)

  private def toDeferredIndex(logFile: LogFile): IO[DeferredIndex] =
    logFile.deferredIndexCell.evalUpdateAndGet: maybe =>
      maybe.match
        case None =>
          buildIndex(logFile).toAllocated
        case Some(allo) =>
          if logFile.isGzipped &&
            !Files.exists(allo.allocatedThing.file)
          then
            logger.debug(s"${allo.allocatedThing.file.getFileName} deleted — rebuilding index")
            allo.release *> buildIndex(logFile).toAllocated
          else if logFile.isGzipped &&
            allo.allocatedThing.fileSize.exists(_.original != Files.size(logFile.originalFile))
          then
            // Happens when a compressed file has been indexed but decompression hasn't finished
            logger.debug:
              s"${logFile.originalFile.getFileName} changed its size — rebuilding index"
            allo.release *> buildIndex(logFile).toAllocated
          else
            IO.pure(allo)
      .map(Some(_))
    .map(_.get.allocatedThing)

  private def buildIndex(logFile: LogFile): ResourceIO[DeferredIndex] =
    if logFile.isGzipped then
      buildIndexFromCompressedFile(logFile.originalFile)
    else
      buildIndexFromUncompressedFile(logFile.originalFile)

  private def buildIndexFromUncompressedFile(file: Path): ResourceIO[DeferredIndex] =
    pollDuration match
      case None =>
        Resource.eval:
          LogFileIndex.fromFile(file).map: logFileIndex =>
            DeferredIndex(logFileIndex, file)
      case Some(poll) =>
        LogFileIndex.buildGrowing(file, poll = poll).map: logFileIndex =>
          DeferredIndex(logFileIndex, file)

  private def buildIndexFromCompressedFile(gzFile: Path): ResourceIO[DeferredIndex] =
    val size = Files.size(gzFile)
    val decompressedFile = Paths.get(gzFile.toString + TmpSuffix)
    Resource.make(
        acquire =
          IO.interruptible:
            val t = Deadline.now
            new GZIPInputStream(
              new BufferedInputStream(
                new FileInputStream(gzFile.toFile))
            ).use: in =>
              Files.copy(in, decompressedFile, REPLACE_EXISTING)
            logger.debug(s"Decompressed ${decompressedFile.getFileName}: ${
              bytesPerSecondString(t.elapsed, Files.size(decompressedFile))}"))(
        release = _ =>
          IO.blocking:
            FileDeleter.tryDeleteFile(decompressedFile))
      .evalMap: _ =>
        LogFileIndex.fromFile(decompressedFile)
          .map: logFileIndex =>
            val decompressedSize = Files.size(decompressedFile)
            Bean.decompressedFilesSize += decompressedSize
            DeferredIndex(logFileIndex, decompressedFile, Some(size -> decompressedSize))

  def files: Seq[Path] =
    instantToLogFile.values.asScala.toVector.map(_.originalFile)

  override def toString =
    s"LogDirectoryIndex($logLevel, ${instantToLogFile.size} files)"


object LogDirectoryIndex:
  private val logger = Logger[LogDirectoryIndex]
  private val TmpSuffix = "-decompressed.tmp"
  private val LogGzTmpSuffix = ".log.gz" + TmpSuffix

  def js7Directory(
    logLevel: LogLevel,
    config: Config,
    poll: Option[FiniteDuration] = None)
    (using zoneId: ZoneId)
  : ResourceIO[LogDirectoryIndex] =
    assertThat(logLevel == Info || logLevel == Debug)
    val currentLogFile = config.as[Path]:
      if logLevel == Debug then "js7.log.debug.file" else "js7.log.info.file"
    val currentLogFilename = currentLogFile.getFileName

    def isValidFile(file: Path) =
      val filename = file.getFileName
      val name = filename.toString
      filename == currentLogFilename ||
        !name.startsWith("~") &&
          !name.endsWith("~") &&
          logLevelMatches(file, logLevel) &&
          isCompressedLogFile(file)

    directory(currentLogFile.getParent, logLevel, isValidFile, config, poll)
  end js7Directory

  def directory(
    directory: Path,
    logLevel: LogLevel,
    isValidFile: Path => Boolean,
    config: Config,
    poll: Option[FiniteDuration] = None)
    (using zoneId: ZoneId)
  : ResourceIO[LogDirectoryIndex] =
    assertThat(logLevel == Info || logLevel == Debug)
    logger.traceResource:
      Resource.suspend:
        deleteTmpFiles(directory, logLevel) *>
          directory.directoryStream[IO]
            .filter: file =>
              !isOurTmpFile(file) && isValidFile(file)
            .compile.toVector.map: files =>
              val directoryEvents = DirectoryWatch.stream(
                directory,
                DirectoryState(files.map(_.getFileName)),
                DirectoryWatchSettings.fromConfig(config).orThrow,
                isValidFile,
                Set(ENTRY_CREATE, ENTRY_DELETE))
              resource(files, directory, directoryEvents, logLevel, poll)

  def files(files: Iterable[Path], logLevel: LogLevel, poll: Option[FiniteDuration] = None)
    (using zoneId: ZoneId)
  : ResourceIO[LogDirectoryIndex] =
    resource(files, Paths.get(".")/*not used*/, Stream.empty, logLevel, poll)

  private def resource(
    files: Iterable[Path],
    logDirectory: Path,
    directoryEvents: Stream[IO, DirectoryEvent],
    logLevel: LogLevel,
    poll: Option[FiniteDuration])
    (using zoneId: ZoneId)
  : ResourceIO[LogDirectoryIndex] =
    Resource.suspend:
      toLogFiles(files).map: logFiles =>
        Service:
          val ourDirEvents = directoryEvents
            .filterNot(o => isOurTmpFile(o.relativePath)) // to be sure
            .map:
              case DirectoryEvent.FileAdded(file) => FileAdded(logDirectory.resolve(file))
              case DirectoryEvent.FileDeleted(file) => FileDeleted(logDirectory.resolve(file))
              case o: DirectoryEvent.FileModified => sys.error(s"LogDirectoryIndex: unexpected $o")
          new LogDirectoryIndex(logFiles, ourDirEvents, logLevel, poll)

  /** Extract the timestamp of the first line of each file and return a sequence of [[LogFile]].
    */
  private def toLogFiles(files: Iterable[Path])(using zoneId: ZoneId): IO[Vector[LogFile]] =
    fs2.Stream.iterable(files)
      .parEvalMapUnordered(sys.runtime.availableProcessors): file =>
        toLogFile(file)
      .compile.toVector
      .map(_.flatten)

  /** Extract the timestamp of the first line of a log file and return a [[LogFile]].
    */
  private def toLogFile(file: Path)(using zoneId: ZoneId): IO[Option[LogFile]] =
    AtomicCell[IO].of(none[Allocated[IO, DeferredIndex]]).flatMap: cell =>
      val size = Files.size(file)
      locally:
        if file.getFileName.toString.endsWith(".gz") then
          readLogFileInstant:
            IO:
              new GZIPInputStream(
                new BufferedInputStream(new FileInputStream(file.toFile), 512))
          .map: maybeInstant =>
            maybeInstant.fold(none): instant =>
              Some(LogFile(instant, file, cell, zoneId, isGzipped = true))
        else
          readLogFileInstant:
            IO:
              new BufferedInputStream(new FileInputStream(file.toFile), UniqueHeaderSize)
          .flatMap:
            _.traverse: instant =>
              LogFileIndex.fromFile(file).map: logFileIndex =>
                LogFile(instant, file, cell, zoneId = zoneId)
      .map: maybe =>
        maybe.foreach: logFile =>
          logger.debug(s"$logFile size=$size")
        maybe
    .handleError: throwable =>
      logger.debug(s"toLogFile ${file.getFileName}: 💥 ${throwable.toStringWithCauses}")
      None

  private def readLogFileInstant(inputStream: IO[InputStream])(using ZoneId): IO[Option[Instant]] =
    Resource.fromAutoCloseable(inputStream).use: in =>
      val array = new Array[Byte](LogFileReader.UniqueHeaderSize)
      IO.blocking:
        ByteArray.unsafeWrap(in.readNBytes(UniqueHeaderSize))
      .map: byteArray =>
        byteArray.length == LogFileReader.UniqueHeaderSize thenMaybe :
          LogFileReader.parseTimestampInHeaderLine(byteArray.utf8String) match
            case EpochNano.Nix => None
            case epochNano => Some(epochNano.toInstant)

  private def deleteTmpFiles(directory: Path, logLevel: LogLevel): IO[Unit] =
    IO.interruptible:
      FileDeleter.tryDeleteFiles:
        directory.directoryStream[SyncIO]
          .filter: file =>
            isOurTmpFile(file) && logLevelMatches(file, logLevel)
          .compile.toVector
          .run()

  private def logLevelMatches(file: Path, logLevel: LogLevel): Boolean =
    (logLevel == Debug) == file.getFileName.toString.contains("-debug")

  private def isCompressedLogFile(file: Path): Boolean =
    file.toString.endsWith(".log.gz")

  private def isOurTmpFile(file: Path): Boolean =
    file.toString.endsWith(LogGzTmpSuffix)

  /** Description a log file and an optional `LogFileIndex`. */
  private final case class LogFile(
    fileInstant: Instant,
    originalFile: Path,
    deferredIndexCell: AtomicCell[IO, Option[Allocated[IO, DeferredIndex]]],
    zoneId: ZoneId,
    isGzipped: Boolean = false):

    val filename: Path =
      originalFile.getFileName

    def release: IO[Unit] =
      deferredIndexCell.getAndSet(None).flatMap:
        _.foldMap: allo =>
          allo.allocatedThing.fileSize.foreach: o =>
            Bean.decompressedFilesSize -= o.decompressed
          allo.release

    def toLogLineKey(logLevel: LogLevel, position: Long): LogLineKey =
      LogLineKey(logLevel, fileInstant, position)

    override def toString =
      f"LogFile(${fileInstant.atZone(zoneId).toOffsetDateTime} -> $filename)"


  /** The deferred LogFileIndex and optionally the temporary decompressed file. */
  private final case class DeferredIndex(
    logFileIndex: LogFileIndex,
    file: Path,
    fileSize: Option[(original: Long, decompressed: Long)] = None)


  private sealed trait OurDirEvent:
    def file: Path

  private final case class FileAdded(file: Path) extends OurDirEvent:
    override def toString = s"FileAdded(${file.getFileName})"

  private final case class FileDeleted(file: Path) extends OurDirEvent:
    override def toString = s"FileDeleted(${file.getFileName})"


  sealed trait LogDirectoryIndexMXBean:
    this: Bean.type =>

    def getDecompressedFilesSize: Long =
      decompressedFilesSize

  object Bean extends LogDirectoryIndexMXBean:
    /*private[LogDirectoryIndex] */var decompressedFilesSize: Long = 0
