package js7.base.log

import cats.effect.{IO, ResourceIO}
import fs2.{Chunk, Stream}
import java.nio.file.Path
import java.time.{Instant, ZoneId}
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.ByteChunksLineSplitter.byteChunksToLines
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.io.file.ByteSeqFileReader
import js7.base.io.file.LogFileReader.parseTimestampInLogLine
import js7.base.log.LogFileIndex.*
import js7.base.metering.CallMeter
import js7.base.time.EpochNano
import js7.base.time.JavaTimeExtensions.toEpochNano
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.bytesPerSecondString
import js7.base.utils.ByteUnits.toKiBGiB
import js7.base.utils.JavaExtensions.toEpochNano
import scala.collection.immutable.VectorBuilder
import scala.concurrent.duration.Deadline

final class LogFileIndex private(
  reader: ByteSeqFileReader[Chunk[Byte]],
  nanoToPos: java.util.NavigableMap[EpochNano, Long],
  zoneId: ZoneId):

  def streamSection(begin: Instant): Stream[IO, Chunk[Byte]] =
    Stream.suspend:
      val toNanos = FastTimestampParser(zoneId)
      val beginEpochNano = begin.toEpochNano
      Stream.exec:
        reader.setPosition(findPositionOf(beginEpochNano))
      .append:
        reader.stream.takeWhile(_.nonEmpty)
          .through:
            byteChunksToLines
          .dropWhile: lineBytes =>
            val line = lineBytes.utf8String
            val epochNano = parseTimestampInLogLine(line, error = -1L)(toNanos.apply)
            epochNano < beginEpochNano

  /** Find the position of the first log line whose timestamp less or equal `beginEpochNano`. */
  private def findPositionOf(beginEpochNano: EpochNano): Long =
    nanoToPos.floorEntry(beginEpochNano) match
      case null => 0 // Start of file
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
  private[log] val BytesPerEntry = 4096 // One index entry per 4KiB-block
  private val MinimumLogDuration = 100.ms
  private val logger = Logger[LogFileIndex]


  private val meterReadFile = CallMeter("LogFileIndex.readChunkFromFile")
  private val meterIndexBuilder = CallMeter("LogFileIndex.buildIndexChunk")

  /** Build an index: negative epochNanos -> byte position of the begin of the line. */
  def resource(logFile: Path, label: String, zoneId: ZoneId = ZoneId.systemDefault)
  : ResourceIO[LogFileIndex] =
    ByteSeqFileReader.resource[Chunk[Byte]](logFile, bufferSize = ByteBufferSize)
      .evalMap: reader =>
        buildIndex(label, zoneId):
          reader.stream.takeWhile(_.nonEmpty)
        .map: nanoToPos =>
          new LogFileIndex(reader, nanoToPos, zoneId)

  private def buildIndex(label: String, zoneId: ZoneId)(stream: Stream[IO, Chunk[Byte]])
  : IO[java.util.NavigableMap[EpochNano, Long]] =
    // TODO Check that timestamp is monotonically increasing.
    IO.defer:
      val toNanos = FastTimestampParser(zoneId)
      val t = Deadline.now
      var byteTotal = 0L
      stream.through:
        byteChunksToLines
      .prefetch
      .scanChunks(PosAndNext(0, 0)):
        case (PosAndNext(position, nextBlock), lines) =>
          val result = new VectorBuilder[NanoAndPos]
          // Compute one NanoAndPos pair for each first position in a block of BytesPerEntry bytes.
          // For each computed NanoAndPos, nextBlock is incremented by BytesPerEntry
          var pos = position
          var nextBlock_ = nextBlock
          var i = 0
          val end = lines.size
          while i < end do // Optimized loop
            val byteLine = lines(i)
            val lineLen = byteLine.size
            byteTotal += lineLen
            if pos >= nextBlock_ then
              val line = byteLine.asciiCharSequence // ASCII !!! is faster than .utf8String
              parseTimestampInLogLine(line)(toNanos.apply) match
                case EpochNano.Nix =>
                case epochNano =>
                  // Insert entry //
                  result += NanoAndPos(epochNano, pos)
                  nextBlock_ = (nextBlock_ + BytesPerEntry max pos + lineLen)
                    / BytesPerEntry * BytesPerEntry
            pos += lineLen
            i += 1
          end while
          PosAndNext(pos, nextBlock_) -> fs2.Chunk.from(result.result())
      .chunks.evalTap: _ =>
        IO.cede
      .unchunks
      .prefetch
      .compile
      .fold(new java.util.TreeMap[EpochNano, Long]): (treeMap, nanoAndPos) =>
        treeMap.put(nanoAndPos.epochNano, nanoAndPos.pos)
        treeMap
      .map: treeMap =>
        val elapsed = t.elapsed
        if elapsed >= MinimumLogDuration then
          logger.debug(s"$label: ${bytesPerSecondString(elapsed, byteTotal)} indexed")
        if treeMap.isEmpty then
          logger.debug(s"❓ buildIndex returns an empty index, no timestamp found in $label")
        treeMap


  // Faster than (_, _) because Long is not being boxed.
  private final case class PosAndNext(pos: Long, nextBlock: Long)

  private final case class NanoAndPos(epochNano: EpochNano, pos: Long)
