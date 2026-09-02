package js7.base.log.reader

import cats.effect.{IO, Resource, ResourceIO}
import fs2.{Chunk, Stream}
import java.nio.file.Path
import java.time.ZoneId
import js7.base.fs2utils.ByteChunksLineSplitter.byteChunksToLines
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.fs2utils.StreamExtensions.cedePeriodically
import js7.base.io.OpaquePos
import js7.base.io.file.ByteSeqFileReader
import js7.base.log.Logger.syntax.*
import js7.base.log.reader.LogFileIndexBuilder.*
import js7.base.log.reader.LogFileReader.streamGrowingLogFile
import js7.base.log.{Logger, reader}
import js7.base.time.EpochNano
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.bytesPerSecondString
import js7.base.utils.ScalaUtils.syntax.*
import scala.collection.mutable
import scala.concurrent.duration.{Deadline, FiniteDuration}
import scala.math.Ordered.orderingToOrdered

private final class LogFileIndexBuilder(label: String, breakLinesLongerThan: Int)
  (using zoneId: ZoneId, conf: LogIndexConf):
  private val nanoToPos = new EpochNanoToPos
  import conf.buildBufferSize

  def buildGrowing(logFile: Path, poll: FiniteDuration): ResourceIO[LogFileIndex] =
    logger.traceResource("buildGrowing", logFile):
      ByteSeqFileReader.resource[Chunk[Byte]](logFile, buildBufferSize).flatMap: reader =>
        Resource.make(
          acquire =
            meterIndexing:
              buildIndex(startPosition = 0):
                reader.streamUntilEnd
            .flatMap: _ =>
              buildIndex(reader.position):
                streamGrowingLogFile(reader, logFile, byteChunkSize = buildBufferSize, poll)
              .start
              .map: fiber =>
                fiber -> nanoToPos)(
          release = (fiber, _) => fiber.cancel)
      .map: (_, nanoToPos) =>
        new LogFileIndex(
          toPositionedStream = (opaquePos, forReader) =>
            assert(!forReader.backwards)
            LogFileIndex.positionedStream(
              logFile, opaquePos, forReader.byteChunkSize,
              forReader.growing ? poll),
          nanoToPos,
          breakLinesLongerThan = Some(breakLinesLongerThan),
          label = label)

  def fromStream(
    toBuilderStream: (bufferSize: Int) => Stream[IO, Chunk[Byte]],
    toPositionedStream: (pos: OpaquePos, forReader: LogSelection.ForReader) => Stream[IO, Chunk[Byte]],
    logWriter: ResourceIO[LogWriter])
    (using LogIndexConf)
  : IO[LogFileIndex] =
    meterIndexing:
      buildIndex(startPosition = 0, toBuilderStream(buildBufferSize), logWriter)
    .map: _ =>
      if nanoToPos.isEmpty then
        logger.debug(s"❓ No timestamped line in $label")
      nanoToPos.shrink()
      new LogFileIndex(toPositionedStream, nanoToPos, Some(breakLinesLongerThan), label = label)

  /** Build the index in `nanoToPos`.
    *
    * @return read byte count */
  private def buildIndex(startPosition: Long)(stream: Stream[IO, Chunk[Byte]])
    (using LogIndexConf)
  : IO[Unit] =
    buildIndex(startPosition, stream, LogWriter.Void.resource(startPosition))

  /** Build the index in `nanoToPos`.
    *
    * @return read byte count */
  private def buildIndex(
    startPosition: Long,
    stream: Stream[IO, Chunk[Byte]],
    logWriter: ResourceIO[LogWriter])
    (using conf: LogIndexConf)
  : IO[Unit] =
    case class PosAndNext(pos: Long, nextBlock: Long)
    logWriter.use: logWriter =>
      val timestampParser = FastTimestampParser()
      var lastEpochNano = nanoToPos.lastEpochNano
      var reverseTimeWarned = false
      stream.prefetch
        .through:
          byteChunksToLines(breakLinesLongerThan = Some(conf.logFileIndexLineLength))
        .prefetch
        .scanChunks(PosAndNext(startPosition, startPosition)): (posAndNext, lines) =>
          // Compute one NanoAndPos pair for each first position in a block of LogBytesPerEntry
          // bytes. For each computed NanoAndPos, nextBlock is incremented by LogBytesPerEntry.
          var pos = posAndNext.pos
          var nextBlock = posAndNext.nextBlock
          val writeOps = WriteOpsBuffer(logWriter)
          lines.iterator.foreach: byteLine =>
            val lineLen = byteLine.size
            nanoToPos.byteCount += lineLen
            if pos >= nextBlock then
              val epochNano = timestampParser.parseTimestampInLogLine(byteLine)
              if !epochNano.isNix then
                if epochNano < lastEpochNano && !reverseTimeWarned then
                  reverseTimeWarned = true
                  logger.warn(s"$label contains a timestamp in reverse order: ${
                    lastEpochNano.show
                  } followed by ${epochNano.show}")
                if epochNano > lastEpochNano then
                  lastEpochNano = epochNano
                  writeOps += epochNano
                val e = conf.logBytesPerEntry
                nextBlock = (nextBlock + e max pos + lineLen) / e * e
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
    private val writeOps = mutable.ArrayBuffer[Chunk[Byte] | EpochNano]()

    def +=(epochNano: EpochNano): Unit =
      writeOps += epochNano

    def +=(chunk: Chunk[Byte]): Unit =
      val length = writeOps.length
      if length == 0 then
        writeOps += chunk
      else
        writeOps(length - 1) match
          case last: Chunk[Byte @unchecked] =>
            // Combine chunks for faster writing, especially when compressing with gzip
            writeOps(length - 1) = last ++ chunk
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


object LogFileIndexBuilder:
  private val logger = Logger[this.type]
