package js7.base.log

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
import js7.base.io.file.LogFileReader.UniqueHeaderSize
import js7.base.io.file.{FileDeleter, LogFileReader}
import js7.base.log.LogDirectoryIndex.*
import js7.base.log.Logger.syntax.*
import js7.base.service.Service
import js7.base.time.EpochNano
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.*
import scala.jdk.CollectionConverters.*

final class LogDirectoryIndex(logDirectory: Path, zoneId: ZoneId)
  extends Service.StoppableByRequest:

  private var instantToLogEntry: java.util.NavigableMap[Instant, LogFile] =
    new java.util.TreeMap

  def start =
    readLogFiles *>
      startService:
        untilStopRequested
          .guarantee:
            release

  private def readLogFiles: IO[Unit] =
    // TODO FileWatch, MapDiff
    logger.debugIO:
      readAll(logDirectory, zoneId).map: logFiles =>
        instantToLogEntry = new java.util.TreeMap(logFiles.toKeyedMap(_.instant).asJava)
        instantToLogEntry.values.asScala.foreach: logFile =>
          logger.debug(s"$logFile")

  private def release: IO[Unit] =
    IO.defer:
      instantToLogEntry.values.asScala.foldMap: logFile =>
        logFile.deferredIndex.evalUpdate:
          case None => IO.none
          case Some(deferredIndex) =>
            deferredIndex.tmpDecompressedFile.foldMap: file =>
              IO.blocking:
                FileDeleter.tryDeleteFile(file)
              .void
            .as(None)

  def streamSection(begin: Instant): Stream[IO, Chunk[Byte]] =
    Stream.suspend:
      if instantToLogEntry.isEmpty then
        Stream.empty
      else
        val logFile = instantToLogEntry.floorEntry(begin) match
          case null => instantToLogEntry.firstEntry().getValue
          case o => o.getValue
        streamAndContinueWithNextFile(begin, logFile)

  private def streamSectionAfter(instant: Instant): Stream[IO, Chunk[Byte]] =
    Stream.suspend:
      Option:
        instantToLogEntry.higherEntry(instant)
      .map(_.getValue).fold(Stream.empty): logFile =>
        streamAndContinueWithNextFile(instant, logFile)

  private def streamAndContinueWithNextFile(instant: Instant, logFile: LogFile) =
    Stream.eval:
      toDeferredIndex(logFile)
    .flatMap: deferredIndex =>
      deferredIndex.logFileIndex.streamLines(instant)
    .append:
      streamSectionAfter(logFile.instant)

  private def toDeferredIndex(logFile: LogFile): IO[DeferredIndex] =
    logFile.deferredIndex.evalUpdateAndGet:
      case Some(o) => IO.pure(Some(o))
      case None => toLogDeferred(logFile).map(Some(_))
    .map(_.get)

  private def toLogDeferred(logFile: LogFile): IO[DeferredIndex] =
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

  override def toString = s"LogDirectoryIndex($logDirectory, ${instantToLogEntry.size} files)"

object LogDirectoryIndex:
  private val logger = Logger[LogDirectoryIndex]

  def service(logDirectory: Path, zoneId: ZoneId): ResourceIO[LogDirectoryIndex] =
    Service:
      new LogDirectoryIndex(logDirectory, zoneId)

  private def readAll(logDirectory: Path, zoneId: ZoneId): IO[Vector[LogFile]] =
    def readAll =
      logDirectory.directoryStream[IO]
        .filter: file =>
          val name = file.getFileName.toString
          !name.startsWith(".") && (name.endsWith(".log") || name.endsWith(".log.gz"))
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
          byteArray.length == LogFileReader.UniqueHeaderSize thenMaybe:
            LogFileReader.parseTimestampInHeaderLine(byteArray.utf8String, zoneId) match
              case EpochNano.Nix => None
              case epochNano => Some(epochNano.toInstant)
    readAll
  end readAll


  private final case class LogFile(
    instant: Instant,
    originalFile: Path,
    deferredIndex: AtomicCell[IO, Option[DeferredIndex]],
    zoneId: ZoneId,
    isGzipped: Boolean = false):

    val filename: Path =
      originalFile.getFileName

    override def toString = s"LogFile(${instant.atZone(zoneId).toOffsetDateTime} -> $filename)"

  private final case class DeferredIndex(
    logFileIndex: LogFileIndex,
    file: Path,
    tmpDecompressedFile: Option[Path] = None)
