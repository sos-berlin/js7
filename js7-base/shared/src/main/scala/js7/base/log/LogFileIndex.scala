package js7.base.log

import cats.effect.IO
import fs2.{Chunk, Stream}
import java.nio.file.Path
import java.time.{Instant, ZoneId}
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.ByteChunksLineSplitter.byteChunksToLines
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.io.file.ByteSeqFileReader
import js7.base.io.file.LogFileReader.parseTimestampInLogLine
import js7.base.log.AnsiEscapeCodes.removeHighlights
import js7.base.log.LogFileIndex.*
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

  // TODO LogLineKey braucht den LogFileIndex nicht!

  def streamLines(begin: Instant | LogLineKey, end: Option[Instant] = None)
  : Stream[IO, Chunk[Byte]] =
    streamPosAndLines(begin, end)
      .map(_._2)

  def streamKeyedLines(begin: Instant | LogLineKey, end: Option[Instant] = None)
  : Stream[IO, KeyedLogLine] =
    streamPosAndLines(begin, end).map: (pos, byteLine) =>
      KeyedLogLine(Instant.EPOCH, pos, removeHighlights(byteLine.utf8String))

  def streamPosAndLines(begin: Instant | LogLineKey, end: Option[Instant])
  : Stream[IO, (Long, Chunk[Byte])] =
    Stream.suspend:
      begin match
        case instant: Instant =>
          streamPosAndLinesFromInstant(instant, end)
        case logLineKey: LogLineKey =>
          streamPosAndLinesFromPosition(logLineKey.position)
            .drop(1)
            .through:
              takeUntilInstant(end)

  def streamPosAndLinesFromInstant(begin: Instant, end: Option[Instant] = None)
  : Stream[IO, (Long, Chunk[Byte])] =
    readerStream.flatMap: reader =>
      val toNanos = FastTimestampParser(zoneId)
      val beginEpochNano = begin.toEpochNano
      val position = findPositionOf(beginEpochNano)
      Stream.exec:
        reader.setPosition(position)
      .append:
        reader.stream.takeWhile(_.nonEmpty)
          .through:
            byteChunksToLines
          .scanChunks(position): (pos, lines) =>
            lines.mapAccumulate(pos): (pos, line) =>
              (pos + line.size) -> (pos -> line)
          .dropWhile: (_, byteLine) =>
            parseTimestampInLogLine(byteLine)(toNanos.apply) < beginEpochNano
          .through:
            takeUntilInstant(end)

  def streamPosAndLinesFromPosition(position: Long)
  : Stream[IO, (Long, Chunk[Byte])] =
    readerStream.flatMap: reader =>
      Stream.exec:
        reader.setPosition(position)
      .append:
        reader.stream.takeWhile(_.nonEmpty)
          .through:
            byteChunksToLines
          .scanChunks(position): (pos, lines) =>
            lines.mapAccumulate(pos): (pos, line) =>
              (pos + line.size) -> (pos -> line)

  private def readerStream: Stream[IO, ByteSeqFileReader[Chunk[Byte]]] =
    Stream.resource:
      ByteSeqFileReader.resource[Chunk[Byte]](logFile, bufferSize = ByteBufferSize)


  private def takeUntilInstant(instant: Option[Instant])
  : fs2.Pipe[IO, (Long, Chunk[Byte]), (Long, Chunk[Byte])] =
    stream =>
      instant.fold(stream): instant =>
        val toNanos = FastTimestampParser(zoneId)
        val endEpochNano = instant.toEpochNano
        stream.takeWhile: (_, byteLine) =>
          val epochNano = parseTimestampInLogLine(byteLine)(toNanos.apply)
          epochNano < endEpochNano

  def instantToFilePosition(instant: Instant): IO[Option[Long]] =
    IO.defer:
      val beginNano = instant.toEpochNano
      val toNanos = FastTimestampParser(zoneId)
      streamPosAndLinesFromInstant(instant).head
        .compile.last
        .map(_.map(_._1))

  /** Find the position of the first log line whose timestamp less or equal `beginEpochNano`. */
  private def findPositionOf(beginEpochNano: EpochNano): Long =
    nanoToPos.floorEntry(beginEpochNano) match
      case null => 0 // Start of logFile
      //case null => nanoToPos.firstEntry().getValue
      case o => o.getValue

  override def toString = s"LogFileIndex(${nanoToPos.size}×${toKiBGiB(BytesPerEntry)})"


object LogFileIndex:
  /** Number of bytes to read at once from the file.
    *
    * 1 MB gives good performance for index building.
    *
    * Due to a `prefetch` operation, two of these buffers a used.
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
