package js7.base.log.reader

import cats.effect.std.AtomicCell
import cats.effect.{IO, Resource, ResourceIO}
import cats.syntax.option.none
import fs2.{Chunk, Stream}
import java.io.{EOFException, FileInputStream, FileNotFoundException}
import java.nio.file.{Files, Path, Paths}
import java.time.{Instant, ZoneId}
import java.util.regex.Pattern
import java.util.zip.GZIPInputStream
import js7.base.data.ByteSequence.ops.*
import js7.base.data.{ByteArray, ByteSequence}
import js7.base.fs2utils.Fs2Utils.inputStreamToStream
import js7.base.io.OpaquePos
import js7.base.io.file.FileDeleter
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.log.reader.LogFile.*
import js7.base.log.reader.LogIndex.*
import js7.base.log.reader.recompressors.Recompressor
import js7.base.problem.Problems.{IncompleteLogFileProblem, InvalidTimestampInLogFileProblem}
import js7.base.problem.{Checked, Problem}
import js7.base.time.EpochNano
import js7.base.time.EpochNano.toEpochNano
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.bytesPerSecondString
import js7.base.utils.Allocated
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ByteUnits.toKBGB
import js7.base.utils.CatsUtils.syntax.*
import js7.base.utils.ScalaUtils.syntax.*
import scala.concurrent.duration.{Deadline, FiniteDuration}
import scala.util.Try

/** Description of a log file with its timestamp and a deferred `LogFileIndex`.
  *
  * Only for [[LogIndex]] friend. */
private final class LogFile private(
  val originalFile: Path,
  val fileInstant: Instant,
  val isGzipped: Boolean,
  val deferredIndexCell: AtomicCell[IO, Option[Allocated[IO, DeferredIndex]]])
  (using zoneId: ZoneId, conf: LogIndexConf):

  val filename: Path =
    originalFile.filename

  val fileEpochNano: EpochNano =
    fileInstant.toEpochNano

  def releaseIndex: IO[Unit] =
    releaseIndex(deleteFile = true)

  def releaseIndex(deleteFile: Boolean): IO[Unit] =
    deferredIndexCell.getAndSet(None).flatMap:
      _.foldMap: deferredIndexAlloc =>
        logger.traceIO("releaseIndex", deferredIndexAlloc.allocatedThing.file.getFileName):
          val deferredIndex = deferredIndexAlloc.allocatedThing
          deferredIndex.fileSize.foreach: o =>
            Bean.tmpFilesSize -= o.decompressed
          if deleteFile && deferredIndex.file != originalFile then
            FileDeleter.tryDeleteFile(deferredIndex.file)
          deferredIndexAlloc.release

  def maybeLogFileIndex: IO[Option[LogFileIndex]] =
    deferredIndexCell.get.map(_.map(_.allocatedThing.logFileIndex))

  def toDeferredIndex(pollGrowing: Option[FiniteDuration])(using Recompressor)
  : IO[DeferredIndex] =
    deferredIndexCell.evalUpdateAndGet: maybe =>
      maybe.match
        case None =>
          buildIndex(pollGrowing).toAllocated
        case Some(allo) =>
          if isGzipped && !Files.exists(allo.allocatedThing.file) then
            logger.debug(s"${allo.allocatedThing.file.getFileName} deleted — rebuilding index")
            allo.release *> buildIndex(pollGrowing).toAllocated
          else if isGzipped
            && allo.allocatedThing.fileSize.exists(_.original != Files.size(originalFile))
          then
            // A compressed file has already been indexed while compression hasn't finished.
            // We index the grown file again.
            logger.debug:
              s"${originalFile.getFileName} changed its size — rebuilding index"
            allo.release *> buildIndex(pollGrowing).toAllocated
          else
            IO.pure(allo)
      .map(Some(_))
    .map(_.get.allocatedThing)

  private def buildIndex(pollGrowing: Option[FiniteDuration])(using Recompressor)
  : ResourceIO[DeferredIndex] =
    if isGzipped then
      buildIndexFromCompressedFile
    else
      buildIndexFromUncompressedFile(originalFile, pollGrowing)

  /** Recompresses and indexes the gzFile. */
  private def buildIndexFromCompressedFile(using recompressor: Recompressor)
  : ResourceIO[DeferredIndex] =
    logger.traceResource("buildIndexFromCompressedFile", originalFile.getFileName):
      Resource.suspend:
        IO:
          val t = Deadline.now
          val size = Files.size(originalFile)
          /** tmpFile contains the indexed, decompressed and maybe recompressed log file. */
          val tmpFile = Paths.get(originalFile.toString + LogUtils.TmpSuffix)
          Resource
            .make(
              acquire =
                LogFileIndex.fromStream(
                  label = tmpFile.getFileName.toString,
                  toBuilderStream = toGzipDecompressingStream,
                  toPositionedStream = (pos, forReader) =>
                    assertThat(!forReader.backwards)
                    positionedTmpFileStream(tmpFile, pos, forReader.byteChunkSize),
                  logWriter = recompressor.toLogWriter(tmpFile)
                ).map: logFileIndex =>
                  logger.info(s"Recompressed and indexed ${tmpFile.getFileName}: ${
                    bytesPerSecondString(t.elapsed, logFileIndex.byteCount)}")
                  logFileIndex)(
              release = _ =>
                IO.blocking:
                  FileDeleter.tryDeleteFile(tmpFile))
            .map: logFileIndex =>
              val recompressedSize = Files.size(tmpFile)
              Bean.tmpFilesSize += recompressedSize
              DeferredIndex(logFileIndex, tmpFile, Some(size -> recompressedSize))

  private def buildIndexFromUncompressedFile(file: Path, pollGrowing: Option[FiniteDuration])
  : ResourceIO[DeferredIndex] =
    pollGrowing match
      case Some(poll) =>
        LogFileIndex.buildGrowing(file, poll = poll).map: logFileIndex =>
          DeferredIndex(logFileIndex, file)
      case _ =>
        Resource.eval:
          LogFileIndex.fromFile(file).map: logFileIndex =>
            DeferredIndex(logFileIndex, file)

  def toGzipDecompressingStream(byteChunkSize: Int): Stream[IO, Chunk[Byte]] =
    Stream.resource:
      Resource.fromAutoCloseable:
        IO.blocking:
          GZIPInputStream(
            FileInputStream(originalFile.toFile),
            byteChunkSize / 8 /*compression ratio*/)
    .flatMap: in =>
      inputStreamToStream(in, bufferSize = byteChunkSize)

  def toStringWithSize: String =
    s"$toString ${Try(toKBGB(Files.size(originalFile))).fold(identity, identity)}"

  override def toString =
    s"LogFile(${fileInstant.atZone(zoneId).toOffsetDateTime} -> $filename)"


private object LogFile:
  private val logger = Logger[LogFile]
  /** First chunk of log file must include the timestamp of the second line
    * (the line after the header) */
  private val HeaderChunkSize = 1024
  private val LogHeaderPattern = Pattern.compile(s"(${FastTimestampParser.DateTimeRegex}) Begin ")
  given Ordering[LogFile] = Ordering.by(_.fileInstant)

  /** Extract the timestamp of the first line of a log file and return a [[LogFile]].
    */
  def read(file: Path)(using ZoneId, LogIndexConf): IO[Checked[LogFile]] =
    val gzip = isGzipped(file)
    readLogFileInstant(file, gzip).flatMapT: instant =>
      AtomicCell[IO].of(none[Allocated[IO, DeferredIndex]]).map: cell =>
        Right:
          LogFile(file, instant, isGzipped = gzip, cell)

  private def readLogFileInstant(file: Path, gzip: Boolean)
    (using zoneId: ZoneId, conf: LogIndexConf)
  : IO[Checked[Instant]] =
    Resource.fromAutoCloseable:
      IO.blocking:
        if gzip then
          GZIPInputStream(FileInputStream(file.toFile), HeaderChunkSize)
        else
          FileInputStream(file.toFile)
    .use: in =>
      IO.blocking:
        ByteArray.unsafeWrap:
          in.readNBytes(HeaderChunkSize)
    .map: chunk =>
      chunk.indexOf('\n') match
        case firstLineEnd if firstLineEnd >= conf.headerMinimumLength =>
          locally:
            if isHeaderLine(chunk.slice(0, firstLineEnd + 1)) then
              chunk.indexOf('\n', firstLineEnd + 1) match
                case -1 => Left(IncompleteLogFileProblem(file))
                case secondLineEnd => Right(chunk.slice(firstLineEnd + 1, secondLineEnd))
            else
              logger.debug(s"No header line in ${file.getFileName}")
              Right(chunk.slice(0, firstLineEnd))
          .flatMap: logLine =>
            // Timestamp of first log line after the header line
            FastTimestampParser()
              .parseTimestampInLogLine(logLine)
              .toOption.toRight:
                InvalidTimestampInLogFileProblem(file, logLine.utf8StringTruncateAt(30))
              .map(_.toInstant)
        case _ =>
          Left(IncompleteLogFileProblem(file))
    .recover:
      case _: EOFException => Left(IncompleteLogFileProblem(file))
      case _: FileNotFoundException => Left(IncompleteLogFileProblem(file))
      case t =>
        if t.getStackTrace != null then
          logger.debug(s"❓readLogFileInstant ${file.getFileName}: ${t.toStringWithCauses}", t)
        Left(Problem.fromThrowable(t))

  private[reader] def isHeaderLine[ByteSeq: ByteSequence](line: ByteSeq): Boolean =
    LogHeaderPattern.matcher(line.asciiCharSequence).lookingAt()

  private def positionedTmpFileStream(file: Path, opaquePos: OpaquePos, bufferSize: Int)
    (using recompressor: Recompressor)
  : Stream[IO, Chunk[Byte]] =
    //Logger.traceStream(s"### positionedTmpFileStream(${file.getFileName})"):
      Stream.resource:
        Resource.fromAutoCloseable:
          IO.blocking:
            FileInputStream(file.toFile)
      .evalMap: (in: FileInputStream) =>
        IO.blocking:
          //Logger.trace(s"### readLogFileInstant ${file.getFileName}: skip($opaquePos)")
          in.skip(opaquePos.toLong)
          recompressor.decompressingInputStream(in)
      .flatMap: in =>
        inputStreamToStream(in, bufferSize)

  /** The deferred LogFileIndex and optionally the temporary decompressed file. */
  final case class DeferredIndex(
    logFileIndex: LogFileIndex,
    file: Path,
    fileSize: Option[(original: Long, decompressed: Long)] = None)
