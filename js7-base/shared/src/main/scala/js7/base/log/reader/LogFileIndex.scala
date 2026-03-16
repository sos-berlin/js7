package js7.base.log.reader

import cats.effect.{IO, ResourceIO}
import fs2.{Chunk, Stream}
import java.nio.file.Path
import java.time.{Instant, ZoneId}
import js7.base.fs2utils.ByteChunksLineSplitter.byteChunksToLines
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.fs2utils.StreamExtensions.cedePeriodically
import js7.base.io.file.ByteSeqFileReader
import js7.base.log.Logger
import js7.base.log.reader.LogFileIndex.*
import js7.base.log.reader.LogFileReader.parseTimestampInLogLine
import js7.base.metering.CallMeter
import js7.base.time.EpochNano
import js7.base.time.JavaTimeExtensions.toEpochNano
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.bytesPerSecondString
import js7.base.utils.ByteUnits.toKiBGiB
import js7.base.utils.Missing
import scala.concurrent.duration.Deadline
import scala.math.Ordered.orderingToOrdered

/** An index for a log file's timestamps to the position (offset) in the log file.
  *
  * Too keep LogFileIndex small, only one (timestamp, position) pair is stored for each
  * BytesPerEntry data block.
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
  */
final class LogFileIndex private(logFile: Path, nanoToPos: NanoToPos)(using ZoneId):

  import ByteSeqFileReader.BufferSize as DefaultBufferSize

  def streamLines(
    begin: Instant,
    end: Option[Instant] = None,
    byteChunkSize: Int = DefaultBufferSize)
  : Stream[IO, Chunk[Byte]] =
    Stream.resource:
      logFileReader(byteChunkSize = byteChunkSize)
    .flatMap: logFileReader =>
      streamPosAndLine(logFileReader, begin, end)
        .map(_._2)

  def instantToFilePosition(logFileReader: LogFileReader, instant: Instant): IO[Option[Long]] =
    IO.defer:
      val position = findBlockPositionOf(instant.toEpochNano)
      streamPosAndLine(logFileReader, instant).head
        .compile.last
        .map(_.map(_._1))

  def streamPosAndLine(logFileReader: LogFileReader, begin: Instant, end: Option[Instant] = None)
  : Stream[IO, (Long, Chunk[Byte])] =
    Stream.suspend:
      val toNanos = FastTimestampParser()
      val beginEpochNano = begin.toEpochNano
      val position = findBlockPositionOf(begin.toEpochNano)
      logFileReader.streamPosAndLines(position)
        .dropWhile: (_, byteLine) =>
          parseTimestampInLogLine(byteLine)(toNanos.apply) < beginEpochNano
        .through:
          logFileReader.takeUntilInstant(end, toNanos)

  /** Find the position of the first log line whose timestamp less or equal `beginEpochNano`. */
  private def findBlockPositionOf(beginEpochNano: EpochNano): Long =
    nanoToPos.toPos(beginEpochNano)

  def logFileReader(byteChunkSize: Int = DefaultBufferSize): ResourceIO[LogFileReader] =
    LogFileReader.resource(logFile, byteChunkSize = byteChunkSize)

  override def toString =
    s"LogFileIndex(${nanoToPos.length}×${toKiBGiB(BytesPerEntry)})"


object LogFileIndex:
  /** Number of bytes to read at once from the file.
    *
    * 1 MB gives good performance for index building.
    *
    * Due to two `prefetch` operation, three times as much memory is used.
    */
  private val ByteBufferSize = 1024 * 1024
  private[log] inline val BytesPerEntry = 4096 // One index entry per 4KiB-block
  private val PositionsPerChunk = ByteBufferSize / BytesPerEntry
  private val MinimumLogDuration = 100.ms
  private val logger = Logger[LogFileIndex]
  private val meterReadFile = CallMeter("LogFileIndex.readChunkFromFile")
  private val meterIndexBuilder = CallMeter("LogFileIndex.buildIndexChunk")
  private val CedePeriod = 10.ms

  /** Build an index: negative epochNanos -> byte position of the begin of the line. */
  def build(logFile: Path, label: String | Missing = Missing)(using ZoneId): IO[LogFileIndex] =
    ByteSeqFileReader.resource[Chunk[Byte]](logFile, bufferSize = ByteBufferSize)
      .use: reader =>
        buildIndex(label getOrElse logFile.getFileName.toString):
          reader.streamUntilEnd
        .map: nanoToPos =>
          new LogFileIndex(logFile, nanoToPos)

  private def buildIndex(label: String)(stream: Stream[IO, Chunk[Byte]])(using ZoneId)
  : IO[NanoToPos] =
    IO.defer:
      val t = Deadline.now
      val nanoToPos = new NanoToPos
      buildIndex1(stream.takeWhile(_.nonEmpty), nanoToPos, label)
        .compile.last
        .map(_.getOrElse(0L))
        .map: byteTotal =>
          val elapsed = t.elapsed
          if elapsed >= MinimumLogDuration then
            logger.debug(s"$label: ${bytesPerSecondString(elapsed, byteTotal)} indexed")
          if nanoToPos.isEmpty then
            logger.debug(s"❓ buildIndex returns an empty index, no timestamp found in $label")
          nanoToPos.shrink()
          nanoToPos

  private def buildIndex1(stream: Stream[IO, Chunk[Byte]], nanoToPos: NanoToPos, label: String)
    (using ZoneId)
  : Stream[IO, Long] =
    case class PosAndNext(pos: Long, nextBlock: Long)
    Stream.suspend:
      val toNanos = FastTimestampParser()
      var byteTotal = 0L
      var lastEpochNano = EpochNano.Nix
      var reverseTimeWarned = false
      stream.prefetch
        .through:
          byteChunksToLines
        .prefetch
        .scanChunks(PosAndNext(0, 0)):
          case (PosAndNext(position, nextBlock), lines) =>
            // Compute one NanoAndPos pair for each first position in a block of BytesPerEntry bytes.
            // For each computed NanoAndPos, nextBlock is incremented by BytesPerEntry
            var pos = position
            var nextBlock_ = nextBlock
            lines.iterator.foreach: byteLine =>
              val lineLen = byteLine.size
              byteTotal += lineLen
              if pos >= nextBlock_ then
                val epochNano = parseTimestampInLogLine(byteLine)(toNanos.apply)
                if !epochNano.isNix then
                  if epochNano < lastEpochNano && !reverseTimeWarned then
                    reverseTimeWarned = true
                    logger.warn(s"$label contains reverse timestamps, ${
                      lastEpochNano.toInstant} > ${epochNano.toInstant}")
                  if epochNano > lastEpochNano then
                    lastEpochNano = epochNano
                    nanoToPos.add(epochNano, pos)
                  nextBlock_ = (nextBlock_ + BytesPerEntry max pos + lineLen)
                    / BytesPerEntry * BytesPerEntry
              pos += lineLen
            PosAndNext(pos, nextBlock_) -> Chunk.singleton(byteTotal)
        .cedePeriodically(CedePeriod)
