package js7.base.log.reader

import cats.effect.{IO, Resource, ResourceIO}
import fs2.{Chunk, Stream}
import java.nio.file.Path
import java.time.{Instant, ZoneId}
import js7.base.catsutils.Environment.environment
import js7.base.config.Js7Conf
import js7.base.fs2utils.ByteChunksLineSplitter.byteChunksToLines
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.fs2utils.Fs2Utils.toPosAndLines
import js7.base.fs2utils.StreamExtensions.cedePeriodically
import js7.base.io.file.ByteSeqFileReader
import js7.base.log.Logger
import js7.base.log.reader.LogFileIndex.*
import js7.base.log.reader.LogFileReader.parseTimestampInLogLine
import js7.base.time.EpochNano
import js7.base.time.JavaTimeExtensions.toEpochNano
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.bytesPerSecondString
import js7.base.utils.ByteUnits.toKiBGiB
import js7.base.utils.Missing
import js7.base.utils.ScalaUtils.syntax.*
import scala.collection.mutable
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
final class LogFileIndex private(
  toPositionedStream: (pos: Long, bufferSize: Int) => Stream[IO, Chunk[Byte]],
  nanoToPos: EpochNanoToPos,
  breakLinesLongerThan: Option[Int])
  (using ZoneId):

  import ByteSeqFileReader.BufferSize as DefaultBufferSize

  def indexSize: Int =
    nanoToPos.length

  def lastEpochNano: EpochNano =
    nanoToPos.lastEpochNano

  def byteCount: Long =
    nanoToPos.byteCount

  def streamLines(
    begin: Instant,
    end: Option[Instant] = None,
    byteChunkSize: Int = DefaultBufferSize)
  : Stream[IO, Chunk[Byte]] =
    streamPosAndLine(begin, end, byteChunkSize = byteChunkSize)
      .map(_._2)

  def instantToFilePosition(instant: Instant, byteChunkSize: Int = DefaultBufferSize)
  : IO[Option[Long]] =
    streamPosAndLine(instant, byteChunkSize = byteChunkSize).head
      .compile.last
      .map(_.map(_._1))

  def streamPosAndLine(
    begin: Instant,
    end: Option[Instant] = None,
    byteChunkSize: Int = DefaultBufferSize)
  : Stream[IO, (Long, Chunk[Byte])] =
    Stream.suspend:
      val t = Deadline.now
      var droppedLines, droppedBytes = 0L
      val timestampParser = FastTimestampParser()
      val beginEpochNano = begin.toEpochNano
      val position = nanoToPos.toPos(begin.toEpochNano)
      toPositionedStream(position, byteChunkSize)
        .through:
          toPosAndLines(firstPosition = position, breakLinesLongerThan = breakLinesLongerThan)
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
          LogFileReader.takeUntilInstant(end, timestampParser)

  override def toString =
    s"LogFileIndex(${nanoToPos.length}×${toKiBGiB(LogBytesPerEntry)})"


object LogFileIndex:
  /** Number of bytes to read at once from the file.
    *
    * 1 MB gives good performance for index building.
    *
    * Due to three `prefetch` operations, four times as much memory is used.
    */
  private val BuildBufferSize = 1024 * 1024
  /** One index entry per 32KiB-block or 1MiB per GiB log file. */
  private[log] inline val LogBytesPerEntry = 32 * 1024
  private val EntrySize = 16 // Size of a EpochNanoToPos entry (two Longs)
  private val NoEntryWarnThreshold = 128 * 1024
  private val PositionsPerChunk = BuildBufferSize / LogBytesPerEntry
  private val MinimumLogDuration = 100.ms
  private val PollDuration = 100.ms
  private val logger = Logger[LogFileIndex]

  logger.debug(s"Blocksize=${toKiBGiB(LogBytesPerEntry)}, occupying 1/${
    LogBytesPerEntry / EntrySize} memory of a log file's size")

  /** Builds a concurrently updated [[LogFileIndex]] from a growing log file. */
  def buildGrowing(
    logFile: Path,
    label: String | Missing = Missing,
    poll: FiniteDuration = PollDuration)
    (using ZoneId)
  : ResourceIO[LogFileIndex] =
    Resource.suspend:
      environment[Js7Conf].map: js7Conf =>
        Builder(resolveLabel(logFile, label), breakLinesLongerThan = js7Conf.logFileIndexLineLength)
          .buildGrowing(logFile, poll)

  /** Builds a snapshot [[LogFileIndex]] from a log file. */
  def fromFile(logFile: Path, label: String | Missing = Missing)(using ZoneId): IO[LogFileIndex] =
    fromStream(
      resolveLabel(logFile, label),
      toBuilderStream = positionedStream(logFile, 0, _),
      toPositionedStream = positionedStream(logFile, _, _))

  def fromStream(
    label: String,
    toBuilderStream: (bufferSize: Int) => Stream[IO, Chunk[Byte]],
    toPositionedStream: (pos: Long, bufferSize: Int) => Stream[IO, Chunk[Byte]],
    logWriter: ResourceIO[LogWriter] = Resource.eval(IO(LogWriter.Void())))
    (using ZoneId)
  : IO[LogFileIndex] =
    environment[Js7Conf].flatMap: js7Conf =>
      Builder(label, breakLinesLongerThan = js7Conf.logFileIndexLineLength)
        .fromStream(toBuilderStream, toPositionedStream, logWriter)

  private def resolveLabel(logFile: Path, label: String | Missing): String =
    label getOrElse logFile.getFileName.toString


  private final class Builder(label: String, breakLinesLongerThan: Int)(using ZoneId):
    private val nanoToPos = new EpochNanoToPos

    def buildGrowing(logFile: Path, poll: FiniteDuration): ResourceIO[LogFileIndex] =
      ByteSeqFileReader.resource[Chunk[Byte]](logFile, BuildBufferSize).flatMap: reader =>
        Resource.make(
          acquire =
            meter:
              buildIndex(startPosition = 0):
                reader.streamUntilEnd
            .flatMap: _ =>
              buildIndex(reader.position):
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
        new LogFileIndex(
          positionedStream(logFile, _, _),
          nanoToPos,
          breakLinesLongerThan = Some(breakLinesLongerThan))

    def fromStream(
      toBuilderStream: (bufferSize: Int) => Stream[IO, Chunk[Byte]],
      toPositionedStream: (pos: Long, bufferSize: Int) => Stream[IO, Chunk[Byte]],
      logWriter: ResourceIO[LogWriter])
    : IO[LogFileIndex] =
      buildIndex(startPosition = 0, toBuilderStream(BuildBufferSize), logWriter)
        .map: _ =>
          if nanoToPos.isEmpty then
            logger.debug(s"❓ No timestamped line in $label")
          nanoToPos.shrink()
          new LogFileIndex(toPositionedStream, nanoToPos, Some(breakLinesLongerThan))

    /** Build the index in `nanoToPos`.
      *
      * @return read byte count */
    private def buildIndex(startPosition: Long)(stream: Stream[IO, Chunk[Byte]])
    : IO[Unit] =
      buildIndex(startPosition, stream, LogWriter.Void.resource(startPosition))

    /** Build the index in `nanoToPos`.
      *
      * @return read byte count */
    private def buildIndex(
      startPosition: Long,
      stream: Stream[IO, Chunk[Byte]],
      logWriter: ResourceIO[LogWriter])
    : IO[Unit] =
      case class PosAndNext(pos: Long, nextBlock: Long)
      environment[Js7Conf].flatMap: js7Conf =>
        logWriter.use: logWriter =>
          val timestampParser = FastTimestampParser()
          var lineNr = 0L
          var lastEpochNano = EpochNano.Nix
          var reverseTimeWarned = false
          stream.prefetch
            .through:
              byteChunksToLines(breakLinesLongerThan = Some(js7Conf.logFileIndexLineLength))
            .prefetch
            .scanChunks(PosAndNext(startPosition, startPosition)): (posAndNext, lines) =>
              // Compute one NanoAndPos pair for each first position in a block of LogBytesPerEntry
              // bytes. For each computed NanoAndPos, nextBlock is incremented by LogBytesPerEntry.
              var pos = posAndNext.pos
              var nextBlock = posAndNext.nextBlock
              val writeOps = WriteOpsBuffer(logWriter)
              lines.iterator.foreach: byteLine =>
                lineNr += 1
                val lineLen = byteLine.size
                nanoToPos.byteCount += lineLen
                if pos >= nextBlock then
                  val epochNano = parseTimestampInLogLine(byteLine)(timestampParser.parse)
                  if !epochNano.isNix then
                    if epochNano < lastEpochNano && !reverseTimeWarned then
                      reverseTimeWarned = true
                      logger.warn(s"$label${(startPosition == 0) ?? s":$lineNr"
                        } contains a timestamp in reverse order: ${
                        lastEpochNano.show} followed by ${epochNano.show}")
                    if epochNano > lastEpochNano then
                      lastEpochNano = epochNano
                      writeOps += epochNano
                    nextBlock = (nextBlock + LogBytesPerEntry max pos + lineLen)
                      / LogBytesPerEntry * LogBytesPerEntry
                end if
                pos += lineLen
                writeOps += byteLine
              PosAndNext(pos, nextBlock) -> Chunk.singleton(writeOps)
            .cedePeriodically
            .prefetch
            .evalMapChunk:
              _.flush
            .compile.drain

    private def meter(body: IO[Unit]): IO[Unit] =
      IO.defer:
        val t = Deadline.now
        body.map: _ =>
          val elapsed = t.elapsed
          if elapsed >= MinimumLogDuration then
            logger.debug(s"$label: ${bytesPerSecondString(elapsed, nanoToPos.byteCount)} indexed")


    private final class WriteOpsBuffer(logWriter: LogWriter):
      val writeOps = mutable.Buffer[Chunk[Byte] | EpochNano]()

      def +=(epochNano: EpochNano): Unit =
        writeOps += epochNano

      def +=(chunk: Chunk[Byte]): Unit =
        writeOps.lastOption match
          case Some(last: Chunk[Byte @unchecked]) =>
            // Combine chunks for faster writing, especially when compressing with gzip
            writeOps(writeOps.length - 1) = last ++ chunk
          case _ =>
            writeOps += chunk

      def flush: IO[Unit] =
        IO.blocking:
          writeOps.foreach:
            case chunk: Chunk[Byte @unchecked] =>
              logWriter.write(chunk)
            case epochNano: EpochNano @unchecked =>
              val markedPos = logWriter.markPosition()
              nanoToPos.add(epochNano, markedPos)

    end WriteOpsBuffer
  end Builder


  /** Allows writing a compressed file and marks seekable positions in this file.
    *
    * For easier handling, despite I/O, the methods don't return IO.
    * The calls are expected to be fast and done only during indexing.
    */
  trait LogWriter:
    def write(chunk: Chunk[Byte]): Unit

    /** Mark the current position as positionable and return the position.
      *
      * A simple file would simply return the written byte count (see also the [[Void]] subclass)
      *
      * For a compress file, the compression would be finished.
      * The following data must be decompressable without knowledge of the forgoing data.
      */
    def markPosition(): Long

  object LogWriter:
    final class Void(startPosition: Long = 0) extends LogWriter:
      private var position = startPosition

      def write(chunk: Chunk[Byte]) =
        position += chunk.size

      def markPosition() =
        position

    object Void:
      def resource(startPosition: Long = 0): ResourceIO[Void] =
        Resource.eval(IO(LogWriter.Void(startPosition)))
  end LogWriter


  def positionedStream(file: Path, position: Long, bufferSize: Int): Stream[IO, Chunk[Byte]] =
    Stream.resource:
      ByteSeqFileReader.resource[Chunk[Byte]](file, bufferSize = bufferSize)
        .evalTap: reader =>
          reader.setPosition(position)
    .flatMap:
      _.streamUntilEnd
