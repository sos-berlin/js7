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
import js7.base.io.OpaquePos
import js7.base.io.file.ByteSeqFileReader
import js7.base.log.reader.LogFileIndex.*
import js7.base.log.reader.LogFileReader.parseTimestampInLogLine
import js7.base.log.reader.LogFileUtils.applyLogSelection
import js7.base.log.{Logger, reader}
import js7.base.time.EpochNano
import js7.base.time.EpochNano.toEpochNano
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
  toPositionedStream: (pos: OpaquePos, byteChunkSize: Int) => Stream[IO, Chunk[Byte]],
  nanoToPos: EpochNanoToPos,
  breakLinesLongerThan: Option[Int])
  (using val zoneId: ZoneId):

  def indexSize: Int =
    nanoToPos.length

  def lastEpochNano: EpochNano =
    nanoToPos.lastEpochNano

  def byteCount: Long =
    nanoToPos.byteCount

  def instantToFilePosition(instant: Instant, logSelection: LogSelection): IO[Option[Long]] =
    streamPosAndLine(instant, byteChunkSize = logSelection.byteChunkSize)
      .through:
        applyLogSelection(logSelection)
      .head
      .compile.last
      .map(_.map(_._1))

  def streamLines(begin: Instant, logSelection: LogSelection): Stream[IO, Chunk[Byte]] =
    streamPosAndLine(begin, byteChunkSize = logSelection.byteChunkSize)
      .through:
        applyLogSelection(logSelection)
      .map(_._2)

  def streamPosAndLine(begin: Instant, byteChunkSize: Int): Stream[IO, PosAndLine] =
    Stream.suspend:
      val t = Deadline.now
      var droppedLines, droppedBytes = 0L
      val timestampParser = FastTimestampParser()
      val beginEpochNano = begin.toEpochNano
      val opaquePos = nanoToPos.toOpaquePos(begin.toEpochNano)
      toPositionedStream(opaquePos, byteChunkSize = byteChunkSize)
        .through:
          toPosAndLines(firstPosition = opaquePos.toLong, breakLinesLongerThan = breakLinesLongerThan)
        .dropWhile: (pos, byteLine) =>
          val drop = parseTimestampInLogLine(byteLine, timestampParser) < beginEpochNano
          if drop then
            droppedLines += 1
            droppedBytes += byteLine.size
          else
            val elapsed = t.elapsed
            logger.trace(s"$droppedLines lines, ${toKiBGiB(droppedBytes)
              } skipped after indexed position · ${elapsed.pretty}")
            val skipped = pos - opaquePos.toLong
            if skipped >= NoEntryWarnThreshold then
              logger.warn(s"Slow direct log file access due to missing index entry for ${
                toKiBGiB(skipped)}, found position=$opaquePos")
          drop
      .map: (pos, line) =>
        PosAndLine(pos, line)

  def positionToLines(position: Long, byteChunkSize: Int): Stream[IO, PosAndLine] =
    Stream.suspend:
      // Convert the byte position of the desired line into the byte position of the corresponding
      // (compressed) chunk and the OpaquePos (the position in the compressed file) of this chunk.
      val (chunkPos, opaquePos) = nanoToPos.posToChunkPosAndOpaquePos(position)
      val t = Deadline.now
      var droppedLines, droppedBytes = 0L
      toPositionedStream(opaquePos, byteChunkSize)
        .through:
          toPosAndLines(firstPosition = chunkPos, breakLinesLongerThan = breakLinesLongerThan)
        .dropWhile: (pos, byteLine) =>
          val drop = pos < position
          if drop then
            droppedLines += 1
            droppedBytes += byteLine.size
          else
            val elapsed = t.elapsed
            logger.trace(s"$droppedLines lines, ${toKiBGiB(droppedBytes)
              } skipped after indexed position · ${elapsed.pretty}")
            if pos - position >= NoEntryWarnThreshold then
              logger.warn(s"Slow direct log file access due to missing index entry for ${
                toKiBGiB(pos - position)
              }, found position=$position")
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
  private val BuildBufferSize = 1024 * 1024
  /** One index entry per 32KiB-block or a half MiB per GiB log file. */
  val LogBytesPerEntry: Int = 32 * 1024
  private val NoEntryWarnThreshold = 128 * 1024
  private val PositionsPerChunk = BuildBufferSize / LogBytesPerEntry
  private val PollDuration = 100.ms
  private val logger = Logger[LogFileIndex]

  logger.debug(s"Blocksize=${toKiBGiB(LogBytesPerEntry)}, requiring 1/${
    LogBytesPerEntry / EpochNanoToPos.EntrySize} of log file's size as heap space")

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
      toBuilderStream = positionedStream(logFile, OpaquePos(0), _),
      toPositionedStream = positionedStream(logFile, _, _))

  def fromStream(
    label: String,
    toBuilderStream: (bufferSize: Int) => Stream[IO, Chunk[Byte]],
    toPositionedStream: (pos: OpaquePos, bufferSize: Int) => Stream[IO, Chunk[Byte]],
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
            meterIndexing:
              buildIndex(startPosition = 0):
                reader.streamUntilEnd
            .flatMap: _ =>
              buildIndex(reader.position):
                reader.streamGrowing(poll)
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
      toPositionedStream: (pos: OpaquePos, bufferSize: Int) => Stream[IO, Chunk[Byte]],
      logWriter: ResourceIO[LogWriter])
    : IO[LogFileIndex] =
      meterIndexing:
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
                  val epochNano = parseTimestampInLogLine(byteLine, timestampParser)
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

    private def meterIndexing(body: IO[Unit]): IO[Unit] =
      IO.defer:
        val t = Deadline.now
        body.map: _ =>
          val elapsed = t.elapsed
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
              val pos = logWriter.position
              val opaquePos = logWriter.markOpaquePos()
              nanoToPos.add(epochNano, opaquePos, pos)

    end WriteOpsBuffer
  end Builder


  private def positionedStream(file: Path, position: OpaquePos, bufferSize: Int): Stream[IO, Chunk[Byte]] =
    ByteSeqFileReader.streamFromPosition(file, position = position.toLong, byteChunkSize = bufferSize)
