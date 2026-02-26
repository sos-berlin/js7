package js7.base.io.file

import cats.effect.{IO, Resource, ResourceIO}
import fs2.{Chunk, Stream}
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.StandardOpenOption.READ
import java.nio.file.{NoSuchFileException, Path}
import js7.base.data.ByteSequence
import js7.base.data.ByteSequence.ops.*
import js7.base.io.file.ByteSeqFileReader.*
import js7.base.log.Logger
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

  extension [ByteSeq](reader: ByteSeqFileReader[ByteSeq])
    def stream: Stream[IO, ByteSeq] =
      Stream.repeatEval:
        reader.read()

    /** Read chunks of ByteSeqs of [[BufferSize]] bytes from the file,
      * while each chunk's byte count is not greater than max(`byteChunkSize`, [[BufferSize]]).
      */
    def chunkStream(byteChunkSize: Int)(using ByteSequence[ByteSeq]): Stream[IO, Chunk[ByteSeq]] =
      chunkStreamX(byteChunkSize = byteChunkSize)
        .takeWhile(_.nonEmpty)

    def chunkStreamX(byteChunkSize: Int)(using ByteSequence[ByteSeq]): Stream[IO, Chunk[ByteSeq]] =
      Stream.repeatEval:
        readChunks(byteChunkSize = byteChunkSize)

    /** Read multiple ByteSeq of [[BufferSize]] bytes from the file until `byteChunkSize` is
      * reached.
      *
      * If [[BufferSize]] is greater than `byteChunkSize`,
      * then one `ByteSeq` of [[BufferSize]] bytes may be returned.
      */
    def readChunks(byteChunkSize: Int, position: Option[Long] = None)
      (using ByteSequence[ByteSeq]): IO[Chunk[ByteSeq]] =
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
