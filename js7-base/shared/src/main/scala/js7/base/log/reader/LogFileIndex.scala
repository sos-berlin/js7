package js7.base.log.reader

import cats.effect.{IO, ResourceIO}
import fs2.{Chunk, Stream}
import java.nio.file.Path
import java.time.{Instant, ZoneId}
import js7.base.fs2utils.ByteChunksLineSplitter.byteChunksToLines
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
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
import scala.collection.mutable
import scala.concurrent.duration.Deadline

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
final class LogFileIndex private(
  logFile: Path,
  nanoToPos: java.util.NavigableMap[EpochNano, Long],
  zoneId: ZoneId):

  import ByteSeqFileReader.BufferSize as DefaultBufferSize

  def streamLines(begin: Instant, end: Option[Instant] = None, byteChunkSize: Int = DefaultBufferSize)
  : Stream[IO, Chunk[Byte]] =
    Stream.resource:
      logFileReader(byteChunkSize = byteChunkSize)
    .flatMap: logFileReader =>
      streamPosAndLines(logFileReader, begin, end)
        .map(_._2)

  def instantToFilePosition(logFileReader: LogFileReader, instant: Instant): IO[Option[Long]] =
    IO.defer:
      val position = findBlockPositionOf(instant.toEpochNano)
      streamPosAndLines(logFileReader, instant).head
        .compile.last
        .map(_.map(_._1))

  def streamPosAndLines(logFileReader: LogFileReader, begin: Instant, end: Option[Instant] = None)
  : Stream[IO, (Long, Chunk[Byte])] =
    Stream.suspend:
      val toNanos = FastTimestampParser(zoneId)
      val beginEpochNano = begin.toEpochNano
      val position = findBlockPositionOf(begin.toEpochNano)
      logFileReader.streamPosAndLines(position)
        .dropWhile: (_, byteLine) =>
          parseTimestampInLogLine(byteLine)(toNanos.apply) < beginEpochNano
        .through:
          logFileReader.takeUntilInstant(end, toNanos)

  /** Find the position of the first log line whose timestamp less or equal `beginEpochNano`. */
  private def findBlockPositionOf(beginEpochNano: EpochNano): Long =
    nanoToPos.floorEntry(beginEpochNano) match
      case null => 0 // Start of logFile
      //case null => nanoToPos.firstEntry().getValue
      case o => o.getValue

  def logFileReader(byteChunkSize: Int = DefaultBufferSize): ResourceIO[LogFileReader] =
    LogFileReader.resource(logFile, zoneId, byteChunkSize = byteChunkSize)

  override def toString =
    s"LogFileIndex(${nanoToPos.size}×${toKiBGiB(BytesPerEntry)})"


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

  /** Build an index: negative epochNanos -> byte position of the begin of the line. */
  def build(
    logFile: Path,
    zoneId: ZoneId = ZoneId.systemDefault,
    label: String | Missing = Missing)
  : IO[LogFileIndex] =
    ByteSeqFileReader.resource[Chunk[Byte]](logFile, bufferSize = ByteBufferSize)
      .use: reader =>
        buildIndex(label getOrElse logFile.getFileName.toString, zoneId):
          reader.stream.takeWhile(_.nonEmpty)
        .map: nanoToPos =>
          new LogFileIndex(logFile, nanoToPos, zoneId)

  private def buildIndex(label: String, zoneId: ZoneId)(stream: Stream[IO, Chunk[Byte]])
  : IO[java.util.NavigableMap[EpochNano, Long]] =
    IO.defer:
      val t = Deadline.now
      buildIndex1(zoneId, stream)
        .map: (treeMap, byteTotal) =>
          val elapsed = t.elapsed
          if elapsed >= MinimumLogDuration then
            logger.debug(s"$label: ${bytesPerSecondString(elapsed, byteTotal)} indexed")
          if treeMap.isEmpty then
            logger.debug(s"❓ buildIndex returns an empty index, no timestamp found in $label")
          treeMap

  private def buildIndex1(zoneId: ZoneId, stream: Stream[IO, Chunk[Byte]])
  : IO[(java.util.NavigableMap[EpochNano, Long], Long)] =
    // TODO Check that timestamp is monotonically increasing.
    case class PosAndNext(pos: Long, nextBlock: Long)
    case class NanoAndPos(epochNano: EpochNano, pos: Long)
    IO.defer:
      val buffer = mutable.ArrayBuilder.ofRef[NanoAndPos]
      val toNanos = FastTimestampParser(zoneId)
      var byteTotal = 0L
      stream.prefetch
        .through:
          byteChunksToLines
        .prefetch
        .scanChunks(PosAndNext(0, 0)):
          case (PosAndNext(position, nextBlock), lines) =>
            buffer.clear()
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
                  buffer += NanoAndPos(epochNano, pos)
                  nextBlock_ = (nextBlock_ + BytesPerEntry max pos + lineLen)
                    / BytesPerEntry * BytesPerEntry
              pos += lineLen
            PosAndNext(pos, nextBlock_) -> Chunk.array(buffer.result())
        .chunkMin(16 * 1024 / PositionsPerChunk /*24 bytes each*/)
        //.prefetch
        .evalTap: _ =>
          IO.cede
        .compile.fold(new java.util.TreeMap[EpochNano, Long]): (treeMap, chunk) =>
          chunk.foreach: nanoAndPos =>
            treeMap.put(nanoAndPos.epochNano, nanoAndPos.pos)
          treeMap
        .map:
          _ -> byteTotal
