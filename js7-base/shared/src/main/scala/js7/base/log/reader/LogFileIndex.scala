package js7.base.log.reader

import cats.effect.{IO, Resource, ResourceIO}
import fs2.{Chunk, Stream}
import java.nio.file.Path
import java.time.{Instant, ZoneId}
import js7.base.catsutils.Environment.environment
import js7.base.config.Js7Conf
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.fs2utils.Fs2Utils.bytesToPosAndLines
import js7.base.io.OpaquePos
import js7.base.io.file.ByteSeqFileReader
import js7.base.log.reader.LogFileIndex.*
import js7.base.log.{Logger, reader}
import js7.base.time.EpochNano
import js7.base.time.EpochNano.toEpochNano
import js7.base.time.ScalaTime.*
import js7.base.utils.Assertions.assertIfStrict
import js7.base.utils.ByteUnits.toKiBGiB
import js7.base.utils.Missing
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.{Deadline, FiniteDuration}
import scala.math.Ordered.orderingToOrdered

/** An index for a log file's timestamps to the position (offset) in the log file.
  *
  * Too keep LogFileIndex small, only one (timestamp, position) pair is stored for each
  * LogBytesPerEntry data block.
  *
  * LogFileIndex provides
  * <ul>
  * <li>the position for a given [[Instant]]
  * <li>stream of lines since an [[Instant]]
  * <li>stream of (position, line) sind an [[Instant]].
  * </ul>
  *
  * The log file is expected to follow a certain layout, see
  * [[js7.base.io.file.LogFileReader]].
  *
  * @param nanoToPos may grow concurrently
  */
final class LogFileIndex private[reader](
  toPositionedStream: (pos: OpaquePos, forReader: LogSelection.ForReader) => Stream[IO, Chunk[Byte]],
  nanoToPos: EpochNanoToPos,
  breakLinesLongerThan: Option[Int])
  (using val zoneId: ZoneId):

  def lastEpochNano: EpochNano =
    nanoToPos.lastEpochNano

  def byteCount: Long =
    nanoToPos.byteCount

  def instantToFilePosition(instant: Instant, logSelection: LogSelection): IO[Option[Long]] =
    instantToLines(instant, logSelection.forReader)
      .through:
        logSelection.pipe
      .head
      .compile.last
      .map(_.map(_._1))

  @TestOnly
  def streamByteLines(begin: Instant, logSelection: LogSelection): Stream[IO, Chunk[Byte]] =
    instantToLines(begin, logSelection.forReader)
      .through:
        logSelection.pipe
      .map(_.byteLine)

  def instantToLines(begin: Instant, forReader: LogSelection.ForReader): Stream[IO, PosAndLine] =
    Stream.suspend:
      val timestampParser = FastTimestampParser()
      val beginEpochNano = begin.toEpochNano
      val (chunkPos, opaquePos) = nanoToPos.epochNanoToChunkPosAndOpaquePos(beginEpochNano)
      toLines(chunkPos, opaquePos, forReader,
        shouldBeDropped = (_, byteLine) =>
          timestampParser.parseTimestampInLogLine(byteLine) < beginEpochNano)

  def positionToLines(position: Long, forReader: LogSelection.ForReader): Stream[IO, PosAndLine] =
    Stream.suspend:
      // Convert the byte position of the desired line into the byte position of the corresponding
      // decompressed chunk and the OpaquePos (the position in the compressed file) of this chunk.
      val (chunkPos, opaquePos) = nanoToPos.posToChunkPosAndOpaquePos(position)
      toLines(chunkPos, opaquePos, forReader,
        shouldBeDropped = (pos, _) => pos < position)

  /**
    * @param chunkPos position of the uncompressed log chunk
    * @param opaquePos position of the (maybe compressed) chunk in file
    */
  private def toLines(
    chunkPos: Long,
    opaquePos: OpaquePos,
    forReader: LogSelection.ForReader,
    shouldBeDropped: (Long, Chunk[Byte]) => Boolean)
  : Stream[IO, PosAndLine] =
    Stream.suspend:
      val t = Deadline.now
      var droppedLines, droppedBytes = 0L
      toPositionedStream(opaquePos, forReader)
        .through:
          bytesToPosAndLines(firstPosition = chunkPos, breakLinesLongerThan = breakLinesLongerThan)
        .dropWhile: (pos, byteLine) =>
          val drop = shouldBeDropped(pos, byteLine)
          if drop then
            droppedLines += 1
            droppedBytes += byteLine.size
          else
            val elapsed = t.elapsed
            logger.trace(s"$droppedLines lines, ${toKiBGiB(droppedBytes)
              } skipped after indexed position · ${elapsed.pretty}")
            val skipped = pos - chunkPos
            if skipped >= NoEntryWarnThreshold then
              logger.warn(s"Slow direct log file access due to missing index entry for ${
                toKiBGiB(skipped)}, found position=$chunkPos")
          drop
        .map: (pos, line) =>
          PosAndLine(pos, line)

  override def toString =
    s"LogFileIndex(${nanoToPos.length}×${toKiBGiB(LogBytesPerEntry)})"


object LogFileIndex:
  /** Number of bytes to read at once from the file.
    *
    * 1 MB gives good performance for index building.
    *
    * Due to three `prefetch` operations, four times as much memory is used.
    */
  private[reader] val BuildBufferSize = 1024 * 1024
  /** One index entry (24 bytes) per 32KiB-block or a 1,4MiB per GiB log file. */
  private[reader] val LogBytesPerEntry: Int = 32 * 1024
  private val NoEntryWarnThreshold = 128 * 1024
  private val PositionsPerChunk = BuildBufferSize / LogBytesPerEntry
  private val PollDuration = 100.ms
  private val logger = Logger[LogFileIndex]

  logger.debug(s"Blocksize=${toKiBGiB(LogBytesPerEntry)}, requiring 1/${
    LogBytesPerEntry / EpochNanoToPos.EntrySize} of a log file's size as heap space")

  /** Builds a concurrently updated [[LogFileIndex]] from a growing log file. */
  def buildGrowing(
    logFile: Path,
    label: String | Missing = Missing,
    poll: FiniteDuration = PollDuration)
    (using ZoneId)
  : ResourceIO[LogFileIndex] =
    Resource.suspend:
      environment[Js7Conf].map: js7Conf =>
        LogFileIndexBuilder(
          resolveLabel(logFile, label),
          breakLinesLongerThan = js7Conf.logFileIndexLineLength
        ).buildGrowing(logFile, poll)

  /** Builds a snapshot [[LogFileIndex]] from a log file. */
  def fromFile(logFile: Path, label: String | Missing = Missing)(using ZoneId): IO[LogFileIndex] =
    fromStream(
      resolveLabel(logFile, label),
      toBuilderStream = positionedStream(logFile, OpaquePos(0), _),
      toPositionedStream = (pos, forReader) =>
        assertIfStrict(!forReader.growing)
        positionedStream(logFile, pos, forReader.byteChunkSize))

  def fromStream(
    label: String,
    toBuilderStream: (bufferSize: Int) => Stream[IO, Chunk[Byte]],
    toPositionedStream: (pos: OpaquePos, forReader: LogSelection.ForReader) => Stream[IO, Chunk[Byte]],
    logWriter: ResourceIO[LogWriter] = Resource.eval(IO(LogWriter.Void())))
    (using ZoneId)
  : IO[LogFileIndex] =
    environment[Js7Conf].flatMap: js7Conf =>
      LogFileIndexBuilder(label, breakLinesLongerThan = js7Conf.logFileIndexLineLength)
        .fromStream(toBuilderStream, toPositionedStream, logWriter)

  private def resolveLabel(logFile: Path, label: String | Missing): String =
    label getOrElse logFile.getFileName.toString

  private[reader] def positionedStream(
    file: Path,
    position: OpaquePos,
    bufferSize: Int,
    pollGrowing: Option[FiniteDuration] = None)
  : Stream[IO, Chunk[Byte]] =
    pollGrowing match
      case None =>
        ByteSeqFileReader.streamFromPosition(
          file, position = position.toLong, byteChunkSize = bufferSize, pollGrowing)
      case Some(poll) =>
        LogFileReader.streamGrowingLogFile[Chunk[Byte]](file, byteChunkSize = bufferSize, poll, position = position.toLong)
