package js7.base.io.file

import cats.effect.{IO, Resource, ResourceIO}
import fs2.Stream
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
      logger.trace(s"setPosition $position")
      _next = null
      channel.position(position)

  def peek: IO[ByteSeq] =
    IO.defer:
      _next match
        case null =>
          readBuffer(buffer).map: r =>
            if r.nonEmpty then
              _next = r
            r
        case byteSeq: ByteSeq @unchecked =>
          IO.pure(byteSeq)

  def read(size: Int): IO[ByteSeq] =
    IO.defer:
      if _next != null then throw IllegalStateException("read(size) called after peek()")
      readBuffer(ByteBuffer.allocate(size))

  def read: IO[ByteSeq] =
    IO.defer:
      _next match
        case null =>
          readBuffer(buffer)
        case byteSeq: ByteSeq @unchecked =>
          _next = null
          IO.pure(byteSeq)

  private def readBuffer(buffer: ByteBuffer): IO[ByteSeq] =
    IO.blocking:
      buffer.clear()
      channel.read(buffer) match
        case -1 =>
          EmptyByteBuffer
        case _ =>
          buffer.flip()
    .map: resultBuffer =>
      ByteSeq.readByteBuffer(resultBuffer)


object ByteSeqFileReader:
  private[io] val BufferSize = 64*1024
  private val logger = Logger[this.type]
  private val EmptyByteBuffer = ByteBuffer.allocate(0)

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
        reader.read
