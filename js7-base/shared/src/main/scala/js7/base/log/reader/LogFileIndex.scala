package js7.base.log.reader

import cats.effect.{IO, Resource, ResourceIO}
import fs2.{Chunk, Stream}
import java.nio.file.Path
import java.time.{Instant, ZoneId}
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.fs2utils.Fs2Utils.bytesToPosAndLines
import js7.base.io.OpaquePos
import js7.base.io.file.ByteSeqFileReader
import js7.base.log.reader.LogFileIndex.*
import js7.base.log.{Logger, reader}
import js7.base.time.EpochNano
import js7.base.time.EpochNano.toEpochNano
import js7.base.time.ScalaTime.*
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ByteUnits.{toKBGB, toKiBGiB}
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
  breakLinesLongerThan: Option[Int],
  label: String)
  (using val zoneId: ZoneId):

  def lastEpochNano: EpochNano =
    nanoToPos.lastEpochNano

  def byteCount: Long =
    nanoToPos.byteCount

  @TestOnly
  def streamByteLines(begin: Instant | Long, logSelection: LogSelection = LogSelection.all)
  : Stream[IO, Chunk[Byte]] =
    streamLines(begin, logSelection).map(_.byteLine)

  @TestOnly
  def streamLines(begin: Instant | Long, logSelection: LogSelection = LogSelection.all)
  : Stream[IO, PosAndLine] =
    begin.match
      case begin: Instant =>
        instantToLines(begin, logSelection.forReader)
      case begin: Long =>
        positionToLines(begin, logSelection.forReader)
    .through:
      logSelection.pipe

  def instantToFilePosition(instant: Instant, logSelection: LogSelection): IO[Option[Long]] =
    instantToLinesForward(instant, logSelection.forReader)
      .through:
        logSelection.pipe
      .head
      .compile.last
      .map(_.map(_._1))

  def instantToLines(begin: Instant, forReader: LogSelection.ForReader): Stream[IO, PosAndLine] =
    if forReader.backwards then
      instantToLinesBackwards(begin, forReader)
    else
      instantToLinesForward(begin, forReader)

  private def instantToLinesForward(begin: Instant, forReader: LogSelection.ForReader)
  : Stream[IO, PosAndLine] =
    Stream.force:
      instantToLinesForward0(begin, forReader)
        .map(_._3)

  private def instantToLinesBackwards(begin: Instant, forReader: LogSelection.ForReader)
  : Stream[IO, PosAndLine] =
    Stream.force:
      // First, convert the instant to a position
      instantToLinesForward0(begin, LogSelection.ForReader(byteChunkSize = LogBytesPerEntry))
        .map: (chunkPos, opaquePos, instantStream) =>
          instantStream.head
            .map(_.position)
            .ifEmpty(Stream.emit(Long.MaxValue))
            .flatMap: position =>
              // Then read backwards from that position
              positionToLinesBackwardsChunk(position, chunkPos, opaquePos, forReader)

  private def instantToLinesForward0(begin: Instant, forReader: LogSelection.ForReader)
  : IO[(Long, OpaquePos, Stream[IO, PosAndLine])] =
    IO:
      val timestampParser = FastTimestampParser()
      val beginEpochNano = begin.toEpochNano
      val (chunkPos, opaquePos) = nanoToPos.epochNanoToChunkPosAndOpaquePos(beginEpochNano)
      val stream =
        toLines(chunkPos, opaquePos, forReader,
          shouldBeDropped = posAndLine =>
            timestampParser.parseTimestampInLogLine(posAndLine.byteLine) < beginEpochNano)
      (chunkPos, opaquePos, stream)

  def positionToLines(position: Long, forReader: LogSelection.ForReader): Stream[IO, PosAndLine] =
    // Convert the byte position of the desired line into the byte position of the corresponding
    // decompressed chunk and the OpaquePos (the position in the compressed file) of this chunk.
    if forReader.backwards then
      positionToLinesBackwards(position, forReader)
    else
      positionToLinesForward(position, forReader)

  private def positionToLinesForward(position: Long, forReader: LogSelection.ForReader) =
    Stream.suspend:
      val (chunkPos, opaquePos) = nanoToPos.posToChunkPosAndOpaquePos(position)
      toLines(chunkPos, opaquePos, forReader, shouldBeDropped = o => o.position < position)

  private def positionToLinesBackwards(position: Long, forReader: LogSelection.ForReader) =
    Stream.suspend:
      val (chunkPos, opaquePos) = nanoToPos.posToChunkPosAndOpaquePos(position)
      positionToLinesBackwardsChunk(position, chunkPos, opaquePos, forReader)

  /** @param skipBackwards Start with 1, then double for each iteration.
    *   This way, it gets faster the more lines are read. But the buffers get bigger. */
  private def positionToLinesBackwardsChunk(
    position: Long, chunkPos: Long, opaquePos: OpaquePos,
    forReader: LogSelection.ForReader,
    skipBackwards: Int = 1)
  : Stream[IO, PosAndLine] =
    Stream.suspend:
      assertThat(chunkPos <= position)
      Stream.eval:
        // Read the indexed chunk forward, then reverse it and continue with the previous chunk.
        toLines(chunkPos, opaquePos,
          // For good speed, BackwardsFileChunkSize should be 1MB (MacBook Pro M4)
          forReader.copyForReader(
            byteChunkSize = BackwardsFileChunkSize min skipBackwards * LogBytesPerEntry,
            backwards = false),
          shouldBeDropped = _ => false
        ).takeWhile(_.position < position)
          .compile.toVector
      .flatMap: vector =>
        Stream.iterable(vector.view.reverse)
      .append:
        if chunkPos == 0 then
          Stream.empty
        else
          val (previousChunkPos, previousOpaquePos) =
            nanoToPos.posToBackwardChunkPosAndOpaquePos(chunkPos, skipBackwards = skipBackwards)
          assertThat(previousChunkPos < chunkPos)
          positionToLinesBackwardsChunk(chunkPos, previousChunkPos, previousOpaquePos, forReader,
            2 * skipBackwards min SkipBackwards)

  /**
    * @param chunkPos position of the uncompressed log chunk
    * @param opaquePos position of the (maybe compressed) chunk in file
    */
  private def toLines(
    chunkPos: Long,
    opaquePos: OpaquePos,
    forReader: LogSelection.ForReader,
    shouldBeDropped: PosAndLine => Boolean)
  : Stream[IO, PosAndLine] =
    assertThat(!forReader.backwards)
    Stream.suspend:
      val t = Deadline.now
      var droppedLines, droppedBytes = 0L
      toPositionedStream(opaquePos, forReader)
        .through:
          bytesToPosAndLines(fromPosition = chunkPos, breakLinesLongerThan = breakLinesLongerThan,
            PosAndLine(_, _))
        .dropWhile: posAndLine =>
          val drop = shouldBeDropped(posAndLine)
          if drop then
            droppedLines += 1
            droppedBytes += posAndLine.byteLine.size
          else
            val elapsed = t.elapsed
            if droppedLines > 0 then
              logger.trace(s"$droppedLines lines, ${toKiBGiB(droppedBytes)
                } skipped after indexed position · ${elapsed.pretty}")
            val skipped = posAndLine.position - chunkPos
            if skipped >= NoEntryWarnThreshold then
              logger.warn(s"Slow direct log file access due to missing index entry for ${
                toKiBGiB(skipped)}, found position=$chunkPos")
          drop

  def wholeFile(forReader: LogSelection.ForReader): Stream[IO, Chunk[Byte]] =
    assertThat(!forReader.backwards)
    toPositionedStream(OpaquePos(0), forReader)

  override def toString =
    s"LogFileIndex($label ${toKiBGiB(nanoToPos.byteCount)} ${toKiBGiB(nanoToPos.memorySize)})"


object LogFileIndex:
  /** One index entry (EpochNanoToPos.EntrySize = 24 bytes) per 256KiB-block.
    *
    * That's 1MB memory per 10GB uncompressed log data. */
  private[reader] val LogBytesPerEntry: Int = 256 * 1024
  private val NoEntryWarnThreshold = 4 * LogBytesPerEntry
  private val BackwardsFileChunkSize = 1024 * 1024 // Backwards read chunks of 1MiB from file
  // Read backwards chunks of up to 4MiB + 1*LogBytesPerEntry
  private val SkipBackwardsSize = 4 * BackwardsFileChunkSize
  private val SkipBackwards = SkipBackwardsSize / LogBytesPerEntry max 1
  private val PollDuration = 100.ms
  private val logger = Logger[LogFileIndex]

  logger.debug(s"Blocksize=${toKiBGiB(LogBytesPerEntry)}, requiring ${
    toKBGB(1_000_000_000L * EpochNanoToPos.EntrySize / LogBytesPerEntry)
  } memory per gigabyte log file")

  /** Builds a concurrently updated [[LogFileIndex]] from a growing log file. */
  def buildGrowing(
    logFile: Path,
    label: String | Missing = Missing,
    poll: FiniteDuration = PollDuration)
    (using zoneId: ZoneId, conf: LogIndexConf)
  : ResourceIO[LogFileIndex] =
    LogFileIndexBuilder(
      resolveLabel(logFile, label),
      breakLinesLongerThan = conf.logFileIndexLineLength
    ).buildGrowing(logFile, poll)

  /** Builds a snapshot [[LogFileIndex]] from a log file. */
  def fromFile(logFile: Path, label: String | Missing = Missing)(using ZoneId, LogIndexConf)
  : IO[LogFileIndex] =
    fromStream(
      resolveLabel(logFile, label),
      toBuilderStream =
        positionedStream(logFile, OpaquePos(0), _),
      toPositionedStream = (pos, forReader) =>
        assertThat(!forReader.backwards)
        positionedStream(logFile, pos, forReader.byteChunkSize))

  def fromStream(
    label: String,
    toBuilderStream: (bufferSize: Int) => Stream[IO, Chunk[Byte]],
    toPositionedStream: (pos: OpaquePos, forReader: LogSelection.ForReader) => Stream[IO, Chunk[Byte]],
    logWriter: ResourceIO[LogWriter] = Resource.eval(IO(LogWriter.Void())))
    (using zoneId: ZoneId, conf: LogIndexConf)
  : IO[LogFileIndex] =
    LogFileIndexBuilder(label, breakLinesLongerThan = conf.logFileIndexLineLength)
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
          file, position = position.toLong, byteChunkSize = bufferSize)
      case Some(poll) =>
        LogFileReader.streamGrowingLogFile[Chunk[Byte]](
          file, byteChunkSize = bufferSize, poll, position = position.toLong)
