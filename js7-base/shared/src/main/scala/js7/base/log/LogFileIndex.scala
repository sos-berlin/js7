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
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.bytesPerSecondString
import js7.base.utils.ByteUnits.toKiBGiB
import js7.base.utils.JavaExtensions.toEpochNanos
import scala.collection.immutable.VectorBuilder
import scala.concurrent.duration.Deadline

final class LogFileIndex private(
  reader: ByteSeqFileReader[Chunk[Byte]],
  nanoToPos: java.util.NavigableMap[Long, Long],
  zoneId: ZoneId):

  def streamSection(start: Instant): Stream[IO, Chunk[Byte]] =
    Stream.suspend:
      val toNanos = FastTimestampParser(zoneId)
      val startEpochNano = start.toEpochNanos
      Stream.exec:
        reader.setPosition(findPositionOf(startEpochNano))
      .append:
        reader.stream.takeWhile(_.nonEmpty)
          .through:
            byteChunksToLines
          .dropWhile: lineBytes =>
            val line = lineBytes.utf8String
            val epochNano = parseTimestampInLogLine(line, error = -1L)(toNanos.apply)
            epochNano < startEpochNano

  /** Find the position of the first log line whose timestamp less or equal `startEpochNano`. */
  private def findPositionOf(startEpochNano: Long): Long =
    nanoToPos.floorEntry(startEpochNano) match
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

  /** Build an index: negative epochNanos -> byte position of the start of the line. */
  def resource(logFile: Path, zoneId: ZoneId = ZoneId.systemDefault): ResourceIO[LogFileIndex] =
    ByteSeqFileReader.resource[Chunk[Byte]](logFile, bufferSize = ByteBufferSize)
      .evalMap: reader =>
        buildIndex(logFile.toString, zoneId):
          reader.stream.takeWhile(_.nonEmpty)
        .map: nanoToPos =>
          new LogFileIndex(reader, nanoToPos, zoneId)

  private def buildIndex(source: String, zoneId: ZoneId)(stream: Stream[IO, Chunk[Byte]])
  : IO[java.util.NavigableMap[Long, Long]] =
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
              parseTimestampInLogLine(line, error = -1L)(toNanos.apply) match
                case -1 =>
                case epochNano =>
                  // Insert entry //
                  result += NanoAndPos(epochNano, pos)
                  nextBlock_ = (nextBlock_ + BytesPerEntry max pos + lineLen)
                    / BytesPerEntry * BytesPerEntry
            pos += lineLen
            i += 1
          end while
          PosAndNext(pos, nextBlock_) -> fs2.Chunk.from(result.result())
      .prefetch
      .compile
      .fold(new java.util.TreeMap[Long, Long]): (treeMap, nanoAndPos) =>
        treeMap.put(nanoAndPos.epochNano, nanoAndPos.pos)
        treeMap
      .map: treeMap =>
        val elapsed = t.elapsed
        if elapsed >= MinimumLogDuration then
          logger.debug(s"$source: ${bytesPerSecondString(elapsed, byteTotal)} indexed")
        if treeMap.isEmpty then
          logger.debug(s"❓ buildIndex returns an empty index, no timestamp found in $source")
        treeMap


  // Faster than (_, _) because Long is not being boxed.
  private final case class PosAndNext(pos: Long, nextBlock: Long)

  private final case class NanoAndPos(epochNano: Long, pos: Long)
