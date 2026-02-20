package js7.base.log

import cats.effect.{IO, Resource, ResourceIO}
import fs2.{Chunk, Stream}
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.Path
import java.nio.file.StandardOpenOption.READ
import java.time.{Instant, ZoneId}
import java.util.regex.Pattern
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.ByteChunksLineSplitter.byteChunksToLines
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.log.AnsiEscapeCodes.HighlightRegex
import js7.base.log.LogFileIndex.*
import js7.base.metering.CallMeter
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.bytesPerSecondString
import js7.base.utils.JavaExtensions.toEpochNanos
import scala.concurrent.duration.Deadline
import scala.util.matching.Regex

final class LogFileIndex private(
  channel: FileChannel,
  nanoToPos: java.util.NavigableMap[Long, Long],
  zoneId: ZoneId):

  def streamFrom(from: Instant): Stream[IO, Chunk[Byte]] =
    Stream.suspend:
      val toNanos = FastTimestampParser(zoneId)
      val fromEpochNano = from.toEpochNanos
      channelToByteChunkStream(channel, from = findPositionOf(fromEpochNano))
        .takeWhile(_.nonEmpty)
        .through:
          byteChunksToLines
        .dropWhile: lineBytes =>
          val line = lineBytes.utf8String
          matchTimestampInLogLine(line) match
            case null => true
            case (start, end) =>
              val epochNano = toNanos(line, start, end) // Returns -1 on error
              epochNano < fromEpochNano

  /** Find the position of the first log line whose timestamp less or equal `fromEpochNano`. */
  private def findPositionOf(fromEpochNano: Long): Long =
    nanoToPos.floorEntry(fromEpochNano) match
      //Doesn't work: case null => 0 // Start of file
      case null => nanoToPos.firstEntry().getValue
      case o => o.getValue

  override def toString = s"LogFileIndex(${nanoToPos.size} entries)}"


object LogFileIndex:
  private val ByteBufferSize = 256 * 1024 // Good performance for index building
  private[log] val BytesPerEntry = 4096 // One index entry per 4KiB-block
  private val MinimumLogDuration = 100.ms
  private val logger = Logger[LogFileIndex]

  private val LogLineRegex: Pattern =
    val datetime = """\d{4}-\d{2}-\d{2}[ Tt]\d{2}:\d{2}:\d{2}[.,]\d{3,6}""".r
    val level = """(?:trace|debug|info|warn|error|INFO|WARN|ERROR)""".r
    val thread = """[\p{Alnum}._$-]+""".r
    val logger = """[\p{Alnum}._$-]+""".r
    val message = """(?:.*)""".r
    Regex(s"""^$HighlightRegex?($datetime) $level +(?:$thread +)?$logger +-""").pattern

  private val meterReadFile = CallMeter("LogFileIndex.readChunkFromFile")
  private val meterRegex = CallMeter("LogFileIndex.buildIndexChunk.regex")
  private val meterIndexBuilder = CallMeter("LogFileIndex.buildIndexChunk")

  /** Build an index: negative epochNanos -> byte position of the start of the line. */
  def resource(logFile: Path, zoneId: ZoneId = ZoneId.systemDefault): ResourceIO[LogFileIndex] =
    openFileChannel(logFile).evalMap: channel =>
      buildIndex(logFile.toString, zoneId):
        channelToByteChunkStream(channel)
          .takeWhile(_.nonEmpty)
      .map: nanoToPos =>
        new LogFileIndex(channel, nanoToPos, zoneId)

  private def buildIndex(source: String, zoneId: ZoneId)(stream: Stream[IO, Chunk[Byte]])
  : IO[java.util.NavigableMap[Long, Long]] =
    IO.defer:
      val toNanos = FastTimestampParser(zoneId)
      val treeMap = new java.util.TreeMap[Long, Long]
      var (t, _byteCount, _lineCount) = (Deadline.now, 0L, 0L)
      stream.through:
        byteChunksToLines
      .compile
      .foldChunks(0L -> 0L):
        case ((position, nextBlock), lines) =>
          // Insert an entry for each first position in a block
          // If an entry has been inserted, nextBlock is incremented by BytesPerEntry
          //_lineCount += lines.size
          lines.foldLeft(position -> nextBlock):
            case ((pos, nextBlock), byteLine) =>
              inline def skip = (pos + byteLine.size, nextBlock)
              _byteCount += byteLine.size
              if pos < nextBlock then
                skip
              else
                val line = byteLine.utf8String
                matchTimestampInLogLine(line) match
                  case null => skip
                  case (start, end) =>
                    val epochNano = toNanos(line, start, end)
                    if epochNano == -1 then
                      skip
                    else
                      // Insert entry //
                      treeMap.putIfAbsent(epochNano, pos)
                      val next = (nextBlock + BytesPerEntry max pos + byteLine.size)
                        / BytesPerEntry * BytesPerEntry
                      (pos + byteLine.size, next)
      .map: _ =>
        val elapsed = t.elapsed
        if elapsed >= MinimumLogDuration then
          //logger.debug(s"$source: ${itemsPerSecondString(elapsed, _lineCount, "lines")}")
          logger.debug(s"$source: ${bytesPerSecondString(elapsed, _byteCount)} indexed")
        if treeMap.isEmpty then
          logger.debug(s"❓ buildIndex returns an empty index, no timestamp found in $source")
        treeMap

  /** Returns timestamp section iff `line` contains a valid log line. */
  private inline def matchTimestampInLogLine(line: String): (Int, Int) | Null =
    meterRegex:
      val matcher = LogLineRegex.matcher(line)
      if matcher.find() then
        val start = matcher.start(1)
        if start >= 0 then
          start -> matcher.end(1)
        else
          null
      else
        null

  /** Outputs an empty Chunk at end of file. */
  def channelToByteChunkStream(channel: FileChannel, from: Long = 0): Stream[IO, Chunk[Byte]] =
    Stream.eval:
      IO.whenA(from > 0):
        IO.blocking:
          channel.position(from)
    .flatMap: _ =>
      val buffer = ByteBuffer.allocate(ByteBufferSize)
      Stream.repeatEval:
        IO.blocking:
          meterReadFile:
            buffer.clear()
            channel.read(buffer) match
              case -1 => Chunk.empty // No more data
              case _ =>
                buffer.flip()
                val array = new Array[Byte](buffer.remaining)
                buffer.get(array)
                Chunk.array(array)

  private def openFileChannel(file: Path): ResourceIO[FileChannel] =
    Resource.fromAutoCloseable:
      IO.blocking:
        FileChannel.open(file, READ)
