package js7.base.log.reader

import cats.effect.std.AtomicCell
import cats.effect.{IO, Resource, ResourceIO}
import cats.syntax.option.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import fs2.Stream
import java.io.{BufferedInputStream, FileInputStream, InputStream}
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.nio.file.{Files, Path, Paths}
import java.time.{Instant, ZoneId}
import java.util.zip.GZIPInputStream
import js7.base.data.ByteArray
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.file.{ByteSeqFileReader, FileDeleter}
import js7.base.log.LogLevel.{Debug, Info}
import js7.base.log.Logger.syntax.*
import js7.base.log.reader.LogDirectoryIndex.*
import js7.base.log.reader.LogFileIndex
import js7.base.log.reader.LogFileReader.UniqueHeaderSize
import js7.base.log.{LogLevel, Logger}
import js7.base.service.Service
import js7.base.time.EpochNano
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.bytesPerSecondString
import js7.base.utils.Allocated
import js7.base.utils.Assertions.assertThat
import js7.base.utils.AutoClosing.syntax.use
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.*
import scala.concurrent.duration.{Deadline, FiniteDuration}
import scala.jdk.CollectionConverters.*

final class LogDirectoryIndex private(
  logFiles: Vector[LogFile],
  logLevel: LogLevel,
  pollDuration: Option[FiniteDuration])
  (using zoneId: ZoneId)
extends Service.TrivialReleasable:

  import ByteSeqFileReader.BufferSize as DefaultBufferSize

  // TODO FileWatch, MapDiff
  private val instantToLogFile: java.util.NavigableMap[Instant, LogFile] =
    new java.util.TreeMap(logFiles.toKeyedMap(_.fileInstant).asJava)

  //instantToLogFile.values.asScala.foreach: logFile =>
  //  logger.debug(s"$logFile")

  protected def release: IO[Unit] =
    IO.defer:
      instantToLogFile.values.asScala.toVector.traverse: logFile =>
        logFile.deferredIndex.getAndSet(None)
      .flatMap:
        _.flatten.parFoldMapA(_.release)

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
        LogFileReader.resource(deferredIndex.file, byteChunkSize = byteChunkSize)
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
      case Some(o) => IO.some(o)
      case None => buildIndex(logFile).toAllocated.map(Some(_))
    .map(_.get.allocatedThing)

  private def buildIndex(logFile: LogFile): ResourceIO[DeferredIndex] =
    val file = logFile.originalFile
    if logFile.isGzipped then
      val decompressedFile = Paths.get(file.toString + ".tmp")
      Resource.make(
        acquire =
          IO.interruptible:
            val t = Deadline.now
            new GZIPInputStream(new BufferedInputStream(new FileInputStream(file.toFile)))
              .use: in =>
                Files.copy(in, decompressedFile, REPLACE_EXISTING)
            logger.debug(s"Decompressed ${decompressedFile.getFileName}: ${
              bytesPerSecondString(t.elapsed, Files.size(decompressedFile))}"))(
        release = _ =>
          IO.blocking:
            FileDeleter.tryDeleteFile(decompressedFile))
      .evalMap: _ =>
        LogFileIndex.build(decompressedFile)
          .map: logFileIndex =>
            DeferredIndex(logFileIndex, decompressedFile)
    else
      pollDuration match
        case None =>
          Resource.eval:
            LogFileIndex.build(file).map: logFileIndex =>
              DeferredIndex(logFileIndex, file)
        case Some(poll) =>
          LogFileIndex.buildGrowing(file, poll = poll).map: logFileIndex =>
            DeferredIndex(logFileIndex, file)

  override def toString =
    s"LogDirectoryIndex($logLevel, ${instantToLogFile.size} files)"


object LogDirectoryIndex:
  private val logger = Logger[LogDirectoryIndex]

  def directory(
    logDirectory: Path,
    logLevel: LogLevel,
    isValidFile: (Path, LogLevel) => Boolean = isValidFile,
    poll: Option[FiniteDuration] = None)
    (using zoneId: ZoneId)
  : ResourceIO[LogDirectoryIndex] =
    logger.traceResource:
      Resource.suspend:
        IO.defer:
          assertThat(logLevel == Info || logLevel == Debug)
          logDirectory.directoryStream[IO]
            .filter: file =>
              isValidFile(file, logLevel)
            .compile.toVector.map: files =>
              this.files(files, logLevel, poll)

  def files(files: Iterable[Path], logLevel: LogLevel, poll: Option[FiniteDuration] = None)
    (using zoneId: ZoneId)
  : ResourceIO[LogDirectoryIndex] =
    Resource.suspend:
      scanFiles(files).map: logFiles =>
        Service:
          new LogDirectoryIndex(logFiles, logLevel, poll)

  /** Extract the timestamp of the first line of each file and return a sequence of [[LogFile]].
    */
  private def scanFiles(files: Iterable[Path])(using zoneId: ZoneId): IO[Vector[LogFile]] =
    def readAll: IO[Vector[LogFile]] =
      fs2.Stream.iterable(files)
        .parEvalMapUnordered(sys.runtime.availableProcessors): file =>
          toLogFile(file).map: maybe =>
            maybe.foreach: logFile =>
              logger.debug(s"$logFile")
            maybe
        .compile.toVector
        .map(_.flatten)

    def toLogFile(file: Path): IO[Option[LogFile]] =
      AtomicCell[IO].of(none[Allocated[IO, DeferredIndex]]).flatMap: cell =>
        if file.getFileName.toString.endsWith(".gz") then
          readInstant:
            IO:
              new GZIPInputStream(
                new BufferedInputStream(new FileInputStream(file.toFile), UniqueHeaderSize))
          .map: maybeInstant =>
            maybeInstant.fold(none): instant =>
              Some(LogFile(instant, file, cell, zoneId, isGzipped = true))
        else
          readInstant:
            IO:
              new BufferedInputStream(new FileInputStream(file.toFile), UniqueHeaderSize)
          .flatMap:
            _.traverse: instant =>
              LogFileIndex.build(file).map: logFileIndex =>
                LogFile(instant, file, cell, zoneId = zoneId)
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
            LogFileReader.parseTimestampInHeaderLine(byteArray.utf8String) match
              case EpochNano.Nix => None
              case epochNano => Some(epochNano.toInstant)

    readAll
  end scanFiles

  def isValidFile(filename: Path, logLevel: LogLevel): Boolean =
    val name = filename.toString
    !name.startsWith(".") &&
      !name.startsWith("~") &&
      (name.endsWith(".log") || name.endsWith(".log.gz")) &&
      ((logLevel == Debug) == name.contains("-debug"))


  /** Description a log file and an optional `LogFileIndex`. */
  private final case class LogFile(
    fileInstant: Instant,
    originalFile: Path,
    deferredIndex: AtomicCell[IO, Option[Allocated[IO, DeferredIndex]]],
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
    file: Path)
