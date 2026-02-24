package js7.base.io.file

import cats.effect.{IO, Resource, ResourceIO}
import fs2.{Chunk, Stream}
import izumi.reflect.Tag
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.StandardOpenOption.READ
import java.nio.file.{Files, NoSuchFileException, Path}
import js7.base.data.ByteSequence
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.StreamExtensions.takeWhileNotNull
import js7.base.io.file.ByteSeqFileReader.*
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixlike.MonixLikeExtensions.onErrorRestartLoop
import js7.base.time.ScalaTime.*
import scala.collection.immutable.VectorBuilder
import scala.concurrent.duration.{Deadline, FiniteDuration}

/** Reads ByteSequences from a file. **/
final class ByteSeqFileReader[ByteSeq] private(file: Path, bufferSize: Int)
  (using ByteSeq: ByteSequence[ByteSeq])
extends AutoCloseable:

  private val channel = FileChannel.open(file, READ)
  private val buffer = ByteBuffer.allocate(bufferSize)
  private var _next: ByteSeq | Null = null
  private var nextPosition: Long = 0

  def close(): Unit =
    channel.close()

  def size(): Long =
    channel.size()

  //def position: Long =
  //  _next match
  //    case null => channel.position
  //    case byteSeq: ByteSeq @unchecked => channel.position - byteSeq.length

  def seekToEnd: IO[Unit] =
    setPosition(size())

  def setPosition(position: Long): IO[Unit] =
    IO.blocking:
      blockingSetPosition(position)

  private[ByteSeqFileReader] def blockingSetPosition(position: Long): Unit =
    logger.trace(s"setPosition $position")
    _next = null
    channel.position(position)

  def peek(): IO[ByteSeq] =
    IO.blocking:
      blockingPeek()

  private[ByteSeqFileReader] def blockingPeek(): ByteSeq =
    _next match
      case null =>
        val r = readBuffer(buffer)
        if r.nonEmpty then
          _next = r
        r
      case byteSeq: ByteSeq @unchecked =>
        byteSeq

  def read(size: Int): IO[ByteSeq] =
    IO.blocking:
      if _next != null then throw IllegalStateException("read(size) called after peek()")
      readBuffer(ByteBuffer.allocate(size))

  def read(position: Option[Long] = None): IO[ByteSeq] =
    IO.blocking:
      position.foreach:
        blockingSetPosition
      blockingRead()

  private[ByteSeqFileReader] def blockingRead(): ByteSeq =
    _next match
      case null =>
        readBuffer(buffer)
      case byteSeq: ByteSeq @unchecked =>
        _next = null
        byteSeq

  private def readBuffer(buffer: ByteBuffer): ByteSeq =
    buffer.clear()
    channel.read(buffer) match
      case -1 =>
        ByteSeq.empty
      case _ =>
        buffer.flip()
        ByteSeq.readByteBuffer(buffer)


object ByteSeqFileReader:
  private[io] val BufferSize = 64*1024
  private val logger = Logger[this.type]

  /** Number of first bytes of a log file with a timestamp which should uniquely identify it.
    *
    * The first line of each log file starts with a timestamp including the timezone offset,
    * to uniquely identify it.
    * <p>
    *   See log4j2.xml header setting. Recommended format:
    * <pre>
    *%d{yyyy-MM-dd HH:mm:ss.SSSXX} ...
    * </pre>
    */
  val UniqueHeaderSize = 30

  def resource[ByteSeq: ByteSequence](
    file: Path,
    bufferSize: Int = BufferSize,
    fromEnd: Boolean = false,
    waitUntilExists: Option[(poll: FiniteDuration, timeout: FiniteDuration)] = None)
  : ResourceIO[ByteSeqFileReader[ByteSeq]] =
    Resource.suspend:
      IO(Deadline.now).map: t =>
        Resource.fromAutoCloseable:
          IO:
            new ByteSeqFileReader[ByteSeq](file, bufferSize = bufferSize)
          .onErrorRestartLoop(()):
            case (e: NoSuchFileException, _, retry) if waitUntilExists.isDefined =>
              val (delay, timeout) = waitUntilExists.get
              if t.elapsed >= timeout then
                IO.raiseError(e)
              else
                logger.debug(s"$file: $e")
                retry(()).delayBy(delay)
            case (t, _, _) => IO.raiseError(t)
        .evalTap: reader =>
          IO.whenA(fromEnd):
            reader.seekToEnd

  def stream[ByteSeq: ByteSequence](file: Path, byteChunkSize: Int): Stream[IO, ByteSeq] =
    Stream.resource:
      resource(file, bufferSize = byteChunkSize)
    .flatMap:
      _.stream
    .takeWhile(_.nonEmpty)

  def growingLogFileStream[ByteSeq: {ByteSequence, Tag}](
    file: Path,
    byteChunkSize: Int,
    pollDuration: FiniteDuration,
    fromEnd: Boolean = false)
  : Stream[IO, Chunk[ByteSeq]] =
    logger.debugStream:
      growingLogFileStream2[ByteSeq](file, byteChunkSize, pollDuration, fromEnd)

  private def growingLogFileStream2[ByteSeq: {ByteSequence, Tag}](
    file: Path,
    byteChunkSize: Int,
    pollDuration: FiniteDuration,
    fromEnd: Boolean = false)
  : Stream[IO, Chunk[ByteSeq]] =
    Stream.resource:
      resource(file, waitUntilExists = Some((poll = pollDuration, timeout = 3.s)))
    .flatMap: reader =>
      Stream.eval:
        reader.read(UniqueHeaderSize).flatTap: _ =>
          if fromEnd then
            reader.seekToEnd
          else
            reader.setPosition(0)
      .flatMap: header =>
        if header.length < UniqueHeaderSize then
          Stream.sleep_[IO](pollDuration) // Delay and end stream. Caller will try again.
        else
          reader.chunkStreamX(byteChunkSize).flatMap: byteSeqs =>
            if byteSeqs.nonEmpty then
              Stream.emit(byteSeqs)
            else
              Stream.sleep_[IO](pollDuration).append:
                // TODO Wait longer before reading header again
                Stream.force:
                  // When the log file chaned, its header file changed, too
                  readHeader[ByteSeq](file).map:
                    case `header` =>
                      Stream.empty // Unchanged, continue
                    case h =>
                      logger.debug(s"Log file header has changed: ${h.utf8String}")
                      Stream.emit(null) // Changed, end the stream
            end if
          .takeWhileNotNull
    .append:
      waitUntilFileExists(file, pollDuration) // TODO Limit waiting time
    .append:
      growingLogFileStream2(file, byteChunkSize, pollDuration)

  private def waitUntilFileExists(file: Path, pollDuration: FiniteDuration): Stream[IO, Nothing] =
    logger.traceCall("waitUntilFileExists", file):
      Stream.eval:
        IO.blocking:
          Files.exists(file)
      .evalTap: exists =>
        IO.unlessA(exists):
          IO.sleep(pollDuration)
      .collectFirst:
        case true =>
      .drain

  private def readHeader[ByteSeq: ByteSequence](file: Path): IO[ByteSeq] =
    logger.traceIO("readHeader"):
      resource[ByteSeq](file, waitUntilExists = Some((poll = 100.ms/*TODO*/, timeout = 3.s))).use: reader =>
        reader.read(UniqueHeaderSize)

  extension [ByteSeq: ByteSequence](reader: ByteSeqFileReader[ByteSeq])
    def stream: Stream[IO, ByteSeq] =
      Stream.repeatEval:
        reader.read()

    /** Read chunks of ByteSeqs of [[BufferSize]] bytes from the file,
      * while each chunk's byte count is not greater than max(`byteChunkSize`, [[BufferSize]]).
      */
    def chunkStream(byteChunkSize: Int): Stream[IO, Chunk[ByteSeq]] =
      chunkStreamX(byteChunkSize = byteChunkSize)
        .takeWhile(_.nonEmpty)

    def chunkStreamX(byteChunkSize: Int): Stream[IO, Chunk[ByteSeq]] =
      Stream.repeatEval:
        readChunks(byteChunkSize = byteChunkSize)

    /** Read multiple ByteSeq of [[BufferSize]] bytes from the file until `byteChunkSize` is
      * reached.
      *
      * If [[BufferSize]] is greater than `byteChunkSize`,
      * then one `ByteSeq` of [[BufferSize]] bytes may be returned.
      */
    def readChunks(byteChunkSize: Int, position: Option[Long] = None): IO[Chunk[ByteSeq]] =
      IO.blocking:
        position.foreach:
          reader.blockingSetPosition

        val b = new VectorBuilder[ByteSeq]
        var size = 0
        while size < byteChunkSize
          && (size == 0 || size + reader.blockingPeek().length <= byteChunkSize)
          && reader.blockingPeek().nonEmpty
        do
          val byteSeq = reader.blockingRead()
          b += byteSeq
          size += byteSeq.length
        Chunk.from(b.result())
