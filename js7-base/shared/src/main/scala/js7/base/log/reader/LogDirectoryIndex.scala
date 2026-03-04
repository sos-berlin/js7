package js7.base.log.reader

import cats.effect.std.AtomicCell
import cats.effect.{IO, Resource, ResourceIO}
import cats.syntax.option.*
import cats.syntax.traverse.*
import fs2.{Chunk, Stream}
import java.io.{BufferedInputStream, FileInputStream, InputStream}
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.nio.file.{Files, Path, Paths}
import java.time.{Instant, ZoneId}
import java.util.zip.GZIPInputStream
import js7.base.data.ByteArray
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.file.{ByteSeqFileReader, FileDeleter}
import js7.base.log.LogLevel.Debug
import js7.base.log.Logger.syntax.*
import js7.base.log.reader.LogDirectoryIndex.*
import js7.base.log.reader.LogFileIndex
import js7.base.log.reader.LogFileReader.UniqueHeaderSize
import js7.base.log.{LogLevel, Logger}
import js7.base.service.Service
import js7.base.time.EpochNano
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.*
import scala.jdk.CollectionConverters.*

final class LogDirectoryIndex private(logLevel: LogLevel, zoneId: ZoneId, logFiles: Vector[LogFile])
extends Service.StoppableByRequest:

  import ByteSeqFileReader.BufferSize as DefaultBufferSize

  // TODO FileWatch, MapDiff
  private val instantToLogFile: java.util.NavigableMap[Instant, LogFile] =
    new java.util.TreeMap(logFiles.toKeyedMap(_.fileInstant).asJava)

  instantToLogFile.values.asScala.foreach: logFile =>
    logger.debug(s"$logFile")

  def start =
    startService:
      untilStopRequested
        .guarantee:
          release

  private def release: IO[Unit] =
    IO.defer:
      instantToLogFile.values.asScala.toVector.traverse: logFile =>
        logFile.deferredIndex.getAndSet(None)
      .map(_.flatten.flatMap(_.tmpDecompressedFile))
      .flatMap: tmpFiles =>
        IO.blocking:
          tmpFiles.foreach: file =>
            FileDeleter.tryDeleteFile(file)

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
      Stream.resource:
        logFileIndex.logFileReader(byteChunkSize = byteChunkSize)
      .flatMap: logFileReader =>
        logFileIndex.streamPosAndLine(logFileReader, instant).map: (pos, line) =>
          KeyedByteLogLine(logFile.toLogLineKey(logLevel, pos), line)
    .append:
      streamSectionAfter(logFile.fileInstant, byteChunkSize = byteChunkSize)

  private def streamAndContinueWithNextFile(logFile: LogFile, position: Long, byteChunkSize: Int)
  : Stream[IO, KeyedByteLogLine] =
    Stream.eval:
      toDeferredIndex(logFile) // TODO DON'T BUILD logFile.logFileIndex. Only decompressed file is required
    .flatMap: deferredIndex =>
      Stream.resource:
        LogFileReader.resource(deferredIndex.file, zoneId, byteChunkSize = byteChunkSize)
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
    logFile.deferredIndex.evalUpdateAndGet:
      case Some(o) => IO.pure(Some(o))
      case None => buildIndex(logFile).map(Some(_))
    .map(_.get)

  private def buildIndex(logFile: LogFile): IO[DeferredIndex] =
    val file = logFile.originalFile
    if logFile.isGzipped then
      val decompressedFile = Paths.get(file.toString + ".tmp")
      logger.debugIO(s"decompress ${logFile.filename}"):
        IO.interruptible:
          autoClosing(
            new GZIPInputStream(new BufferedInputStream(new FileInputStream(file.toFile)))
          ): in =>
            Files.copy(in, decompressedFile, REPLACE_EXISTING)
      .productR:
        LogFileIndex.build(decompressedFile, zoneId = zoneId)
          .map: logFileIndex =>
            DeferredIndex(logFileIndex, decompressedFile, Some(decompressedFile))
    else
      LogFileIndex.build(file, zoneId = zoneId).map: logFileIndex =>
        DeferredIndex(logFileIndex, file)

  override def toString = s"LogDirectoryIndex($logLevel, ${instantToLogFile.size} files)"


object LogDirectoryIndex:
  private val logger = Logger[LogDirectoryIndex]

  def resource(
    logLevel: LogLevel,
    zoneId: ZoneId,
    logDirectory: Path,
    isValidFile: (LogLevel, Path) => Boolean = isValidFile)
  : ResourceIO[LogDirectoryIndex] =
    Resource.suspend:
      logDirectory.directoryStream[IO]
        .filter: file =>
          isValidFile(logLevel, file)
        .compile.toVector.map: files =>
          resource(logLevel, zoneId, files)

  def resource(logLevel: LogLevel, zoneId: ZoneId, files: Iterable[Path])
  : ResourceIO[LogDirectoryIndex] =
    Resource.suspend:
      scanFiles(zoneId, files).map: logFiles =>
        Service:
          new LogDirectoryIndex(logLevel, zoneId, logFiles)

  /** Extract the timestamp of the first line of each file and return a sequence of [[LogFile]].
    */
  private def scanFiles(zoneId: ZoneId, files: Iterable[Path]): IO[Vector[LogFile]] =
    def readAll: IO[Vector[LogFile]] =
      fs2.Stream.iterable(files)
        .parEvalMapUnordered(sys.runtime.availableProcessors): file =>
          toLogFile(file).map: maybe =>
            Chunk.fromOption(maybe)
        .unchunks
        .compile.toVector

    def toLogFile(file: Path): IO[Option[LogFile]] =
      AtomicCell[IO].of(none[DeferredIndex]).flatMap: cell =>
        if file.getFileName.toString.endsWith(".gz") then
          readInstant(IO:
            new GZIPInputStream(
              new BufferedInputStream(new FileInputStream(file.toFile), UniqueHeaderSize))
          ).map: maybeInstant =>
            maybeInstant.fold(none): instant =>
              Some(LogFile(instant, file, cell, zoneId, isGzipped = true))
        else
          readInstant(IO:
            new BufferedInputStream(new FileInputStream(file.toFile), UniqueHeaderSize)
          ).flatMap:
            _.traverse: instant =>
              LogFileIndex.build(file, zoneId = zoneId).map: logFileIndex =>
                LogFile(instant, file, cell, zoneId)
      .handleError: throwable =>
        logger.debug(s"toLogFile ${file.getFileName}: 💥 ${throwable.toStringWithCauses}")
        None

    def readInstant(inputStream: IO[InputStream]): IO[Option[Instant]] =
      Resource.fromAutoCloseable(inputStream).use: in =>
        val array = new Array[Byte](LogFileReader.UniqueHeaderSize)
        IO.blocking:
          ByteArray.unsafeWrap(in.readNBytes(UniqueHeaderSize))
        .map: byteArray =>
          byteArray.length == LogFileReader.UniqueHeaderSize thenMaybe :
            LogFileReader.parseTimestampInHeaderLine(byteArray.utf8String, zoneId) match
              case EpochNano.Nix => None
              case epochNano => Some(epochNano.toInstant)

    readAll
  end scanFiles

  def isValidFile(logLevel: LogLevel, filename: Path): Boolean =
    val name = filename.toString
    !name.startsWith(".") &&
      !name.startsWith("~") &&
      (name.endsWith(".log") || name.endsWith(".log.gz")) &&
      ((logLevel == Debug) == name.contains("-debug"))


  /** Description a log file and an optional `LogFileIndex`. */
  private final case class LogFile(
    fileInstant: Instant,
    originalFile: Path,
    deferredIndex: AtomicCell[IO, Option[DeferredIndex]],
    zoneId: ZoneId,
    isGzipped: Boolean = false):

    val filename: Path =
      originalFile.getFileName

    def toLogLineKey(logLevel: LogLevel, position: Long): LogLineKey =
      LogLineKey(logLevel, fileInstant, position)

    override def toString =
      f"LogFile(${fileInstant.atZone(zoneId).toOffsetDateTime} -> $filename)"


  /** The deferred LogFileIndex and optionally the temporary decompressed file. */
  private final case class DeferredIndex(
    logFileIndex: LogFileIndex,
    file: Path,
    tmpDecompressedFile: Option[Path] = None)
