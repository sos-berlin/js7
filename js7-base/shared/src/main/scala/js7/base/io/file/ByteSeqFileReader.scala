package js7.base.io.file

import cats.effect.{IO, Resource, ResourceIO}
import fs2.Stream
import java.io.InputStream
import java.nio.ByteBuffer
import java.nio.channels.{FileChannel, SeekableByteChannel}
import java.nio.file.StandardOpenOption.READ
import java.nio.file.{NoSuchFileException, Path}
import js7.base.data.ByteSequence
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.Fs2Utils.toPosAndLines
import js7.base.io.file.ByteSeqFileReader.*
import js7.base.log.Logger
import js7.base.monixlike.MonixLikeExtensions.onErrorRestartLoop
import js7.base.time.ScalaTime.*
import scala.annotation.threadUnsafe
import scala.concurrent.duration.{Deadline, FiniteDuration}

/** Reads ByteSequences from a file. **/
final class ByteSeqFileReader[ByteSeq: ByteSequence as ByteSeq] private(
  channel: SeekableByteChannel, bufferSize: Int):

  @threadUnsafe
  private lazy val buffer = ByteBuffer.allocate(bufferSize)
  private var _next: ByteSeq | Null = null
  private var _nextPosition: Long = 0

  def size(): Long =
    channel.size()

  def position: Long =
    _nextPosition

  def seekToEnd: IO[Unit] =
    setPosition(size())

  def setPosition(position: Long): IO[Unit] =
    // TODO Probably it's more efficient to clip next buffer size after positing
    IO.defer:
      if position == _nextPosition then
        IO.unit
      else if position == channel.position then
        _next = null
        IO.unit
      else
        IO.blocking:
          logger.trace(s"setPosition from ${channel.position} to $position")
          _next = null
          channel.position(position)
          _nextPosition = position

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
    .map: byteSeq =>
      _nextPosition = channel.position
      byteSeq

  def read: IO[ByteSeq] =
    IO.defer:
      _next match
        case null =>
          readBuffer(buffer)
        case byteSeq: ByteSeq @unchecked =>
          _next = null
          IO.pure(byteSeq)
    .map: byteSeq =>
      _nextPosition = channel.position
      byteSeq

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
  val BufferSize = 64*1024
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
            FileChannel.open(file, READ)
          .onErrorRestartLoop(()):
            case (e: NoSuchFileException, _, retry) if waitUntilExists.isDefined =>
              val (delay, timeout) = waitUntilExists.get
              if t.elapsed >= timeout then
                IO.raiseError(e)
              else
                logger.debug(s"$file: $e")
                retry(()).delayBy(delay)
            case (t, _, _) => IO.raiseError(t)
        .map: channel =>
          new ByteSeqFileReader[ByteSeq](channel, bufferSize = bufferSize)
        .evalTap: reader =>
          IO.whenA(fromEnd):
            reader.seekToEnd

  def stream[ByteSeq: ByteSequence](file: Path, byteChunkSize: Int): Stream[IO, ByteSeq] =
    Stream.resource:
      resource(file, bufferSize = byteChunkSize)
    .flatMap:
      _.streamUntilEnd


  extension [ByteSeq](reader: ByteSeqFileReader[ByteSeq])

    def streamPosAndLines(position: Long, breakLinesLongerThan: Option[Int])
      (using ByteSeq: ByteSequence[ByteSeq])
    : Stream[IO, (Long, ByteSeq)] =
      fs2.Stream.exec:
        reader.setPosition(position)
      .append:
        streamUntilEnd
      .through:
        toPosAndLines(firstPosition = position, breakLinesLongerThan = breakLinesLongerThan)

    def streamUntilEnd(using ByteSequence[ByteSeq]): Stream[IO, ByteSeq] =
      streamEndlessly.takeWhile(_.nonEmpty)

    /** Emit empty ByteSeqs as long as no data is available.
      *
      * This Stream never ends.
      */
    def streamEndlessly: Stream[IO, ByteSeq] =
      Stream.repeatEval:
        reader.read

    private def inputStream_UNTESTED: InputStream =
      new InputStream:
        private val singleArray = new Array[Byte](1)
          def read() =
            read(singleArray, 0, 1) match
              case -1 => -1
              case 1 => singleArray(0)

        override def read(array: Array[Byte], offset: Int, length: Int) =
          val buffer = ByteBuffer.wrap(array, offset, length)
          reader.readBuffer(buffer)
          if buffer.hasRemaining then
            buffer.remaining
          else
            -1
