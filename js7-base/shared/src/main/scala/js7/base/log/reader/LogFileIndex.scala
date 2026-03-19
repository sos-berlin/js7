package js7.base.log.reader

import cats.effect.{IO, Resource, ResourceIO}
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
final class LogFileIndex private(logFile: Path, nanoToPos: EpochNanoToPos)(using ZoneId):
  import ByteSeqFileReader.BufferSize as DefaultBufferSize

  def indexSize: Int =
    nanoToPos.length

  def lastEpochNano: EpochNano =
    nanoToPos.lastEpochNano

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
    streamPosAndLine(logFileReader, instant).head
      .compile.last
      .map(_.map(_._1))

  def streamPosAndLine(logFileReader: LogFileReader, begin: Instant, end: Option[Instant] = None)
  : Stream[IO, (Long, Chunk[Byte])] =
    Stream.suspend:
      val t = Deadline.now
      var droppedLines, droppedBytes = 0L
      val timestampParser = FastTimestampParser()
      val beginEpochNano = begin.toEpochNano
      val position = nanoToPos.toPos(begin.toEpochNano)
      logFileReader.streamPosAndLines(position)
        .dropWhile: (pos, byteLine) =>
          val drop = parseTimestampInLogLine(byteLine)(timestampParser.parse) < beginEpochNano
          if drop then
            droppedLines += 1
            droppedBytes += byteLine.size
          else
            val elapsed = t.elapsed
            logger.trace(s"$droppedLines lines, ${toKiBGiB(droppedBytes)
              } skipped after indexed position · ${elapsed.pretty}")
            if pos - position >= NoEntryWarnThreshold then
              logger.warn(s"Slow direct log file access due to missing index entry for ${
                toKiBGiB(pos - position)}, found position=$position")
          drop
        .through:
          logFileReader.takeUntilInstant(end, timestampParser)

  def logFileReader(byteChunkSize: Int = DefaultBufferSize): ResourceIO[LogFileReader] =
    LogFileReader.resource(logFile, byteChunkSize = byteChunkSize)

  override def toString =
    s"LogFileIndex(${nanoToPos.length}×${toKiBGiB(LogBytesPerEntry)})"


object LogFileIndex:
  /** Number of bytes to read at once from the file.
    *
    * 1 MB gives good performance for index building.
    *
    * Due to two `prefetch` operation, three times as much memory is used.
    */
  private val ByteBufferSize = 1024 * 1024
  /** One index entry per 16KiB-block or 1MiB per GiB log file. */
  private[log] inline val LogBytesPerEntry = 16 * 1024
  private val EntrySize = 16 // Size of a EpochNanoToPos entry (two Longs)
  private val NoEntryWarnThreshold = 128 * 1024
  private val PositionsPerChunk = ByteBufferSize / LogBytesPerEntry
  private val MinimumLogDuration = 100.ms
  private val PollDuration = 100.ms
  private val logger = Logger[LogFileIndex]
  private val meterReadFile = CallMeter("LogFileIndex.readChunkFromFile")
  private val meterIndexBuilder = CallMeter("LogFileIndex.buildIndexChunk")

  logger.debug(s"Blocksize=${toKiBGiB(LogBytesPerEntry)}, occupying 1/${
    LogBytesPerEntry / EntrySize} memory of a log file's size")

  /** Builds a concurrently updated [[LogFileIndex]] from a growing log file. */
  def buildGrowing(
    logFile: Path,
    label: String | Missing = Missing,
    poll: FiniteDuration = PollDuration)
    (using ZoneId)
  : ResourceIO[LogFileIndex] =
    Builder(logFile, label)
      .buildGrowing(poll)

  /** Builds a snapshot [[LogFileIndex]] from a log file. */
  def build(logFile: Path, label: String | Missing = Missing)(using ZoneId): IO[LogFileIndex] =
    Builder(logFile, label)
      .build()


  private final class Builder private(logFile: Path, label: String)(using ZoneId):
    private val nanoToPos = new EpochNanoToPos

    def buildGrowing(poll: FiniteDuration)(using ZoneId): ResourceIO[LogFileIndex] =
      byteSeqFileReader.flatMap: reader =>
        Resource.make(
          acquire =
            meter:
              buildIndex(startPosition = 0):
                reader.streamUntilEnd
            .flatMap: _ =>
              buildIndex(reader.position, log = true):
                reader.streamEndlessly
                  .flatMap: chunk =>
                    if chunk.nonEmpty then
                      fs2.Stream.emit(chunk)
                    else
                      Stream.sleep_(poll)
              .start
              .map: fiber =>
                fiber -> nanoToPos)(
          release = (fiber, _) => fiber.cancel)
      .map: (_, nanoToPos) =>
        new LogFileIndex(logFile, nanoToPos)

    def build(): IO[LogFileIndex] =
      byteSeqFileReader.use: reader =>
        buildIndex(startPosition = 0):
          reader.streamUntilEnd
        .map: _ =>
          if nanoToPos.isEmpty then
            logger.debug(s"❓ No timestamped line in $label")
          nanoToPos.shrink()
          new LogFileIndex(logFile, nanoToPos)

    private def byteSeqFileReader: ResourceIO[ByteSeqFileReader[Chunk[Byte]]] =
      ByteSeqFileReader.resource[Chunk[Byte]](logFile, bufferSize = ByteBufferSize)

    /** @return read byte count */
    private def buildIndex(startPosition: Long, log: Boolean = false)
      (stream: Stream[IO, Chunk[Byte]])
    : IO[Long] =
      stream.through:
        buildStream(startPosition)
      .compile.last.map(_.getOrElse(0L))

    /** @return read byte count */
    private def buildStream(startPosition: Long): fs2.Pipe[IO, Chunk[Byte], Long] =
      case class PosAndNext(pos: Long, nextBlock: Long)
      stream =>
        val timestampParser = FastTimestampParser()
        var byteTotal = 0L
        var lastEpochNano = EpochNano.Nix
        var reverseTimeWarned = false
        stream.prefetch
          .through:
            byteChunksToLines
          .prefetch
          .scanChunks(PosAndNext(startPosition, startPosition)):
            case (PosAndNext(position, nextBlock), lines) =>
              // Compute one NanoAndPos pair for each first position in a block of LogBytesPerEntry
              // bytes. For each computed NanoAndPos, nextBlock is incremented by LogBytesPerEntry.
              var pos = position
              var nextBlock_ = nextBlock
              lines.iterator.foreach: byteLine =>
                val lineLen = byteLine.size
                byteTotal += lineLen
                if pos >= nextBlock_ then
                  val epochNano = parseTimestampInLogLine(byteLine)(timestampParser.parse)
                  if !epochNano.isNix then
                    if epochNano < lastEpochNano && !reverseTimeWarned then
                      reverseTimeWarned = true
                      logger.warn(s"$label contains reverse timestamps, ${
                        lastEpochNano.show} > ${epochNano.show}")
                    if epochNano > lastEpochNano then
                      lastEpochNano = epochNano
                      nanoToPos.add(epochNano, pos)
                    nextBlock_ = (nextBlock_ + LogBytesPerEntry max pos + lineLen)
                      / LogBytesPerEntry * LogBytesPerEntry
                pos += lineLen
              PosAndNext(pos, nextBlock_) -> Chunk.singleton(byteTotal)
          .cedePeriodically

    private def meter(body: IO[Long]): IO[Unit] =
      IO.defer:
        val t = Deadline.now
        body.map: byteTotal =>
          val elapsed = t.elapsed
          if elapsed >= MinimumLogDuration then
            logger.debug(s"$label: ${bytesPerSecondString(elapsed, byteTotal)} indexed")

  private object Builder:
    def apply(logFile: Path, label: String | Missing = Missing)(using ZoneId): Builder =
      new Builder(logFile, label getOrElse logFile.getFileName.toString)
