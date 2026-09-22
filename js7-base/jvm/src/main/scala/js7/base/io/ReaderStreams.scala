package js7.base.io

import cats.effect.IO
import fs2.{Chunk, Stream}
import java.io.{InputStream, Reader}
import java.nio.ByteBuffer
import java.nio.channels.Channels.newChannel
import java.nio.channels.{AsynchronousCloseException, ReadableByteChannel}
import js7.base.catsutils.CatsEffectExtensions.handleCancel
import js7.base.fs2utils.Fs2ChunkByteSequence
import js7.base.fs2utils.StreamExtensions.takeWhileNotNull
import js7.base.log.Logger
import js7.base.thread.IOExecutor.env.virtualThread
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import org.jetbrains.annotations.TestOnly

object ReaderStreams:

  private val logger = Logger[this.type]
  private val DefaultBufferSize = 8192

  /** Returns a Chunk[Byte] immediately after each read operation. */
  def inputStreamToByteStream(
    in: InputStream,
    bufferSize: Int = DefaultBufferSize,
    label: String = "")
  : Stream[IO, Byte] =
    channelToByteStream(newChannel(in), bufferSize, label)

  def channelToByteStream(
    channel: ReadableByteChannel,
    bufferSize: Int = DefaultBufferSize,
    label: String = "")
  : Stream[IO, Byte] =
    Stream.suspend:
      val buffer = ByteBuffer.allocate(bufferSize)
      @volatile var reading, canceled = false
      Stream.repeatEval:
        virtualThread:
          buffer.clear()
          reading = true
          try channel.read(buffer)
          catch
            case _: AsynchronousCloseException if canceled =>
              0 // Ignored because IO.canceled will be returned
            case e: Exception =>
              logger.debug(s"💥 ${if label.isEmpty then "" else s"$label "}channel.read${
                canceled ?? " (canceled)"}: $e")
              throw e
          finally reading = false
        .handleCancel:
          // Don't interrupt a Windows ReadFile, because interrupt may block.
          // Then Fiber#cancel blocks, too. Instead, we channel.close (Windows CloseFile).
          // CloseFile blocks until ReadFile received has something.
          // Windows: This means the file handle is open until something is received !!!
          // This may consume a all available file handles !!!
          IO: // May happen under Windows when CloseHandle blocks while ReadFile has not completed
            logger.warn(s"$label: 🐌 Cancellation with Channel#close blocks${
              reading ?? " while reading, possibly until something has been received"} ...")
          .delayBy(1.s)
          .background.surround:
            virtualThread:
              canceled = true
              channel.close()
        .map:
          case -1 =>
            null
          case o if o < 1 =>
            throw RuntimeException(s"$label: '$channel'.read returned the unexpected value o $o")
          case _ =>
            buffer.flip()
            Fs2ChunkByteSequence.readByteBuffer(buffer)
      .takeWhileNotNull
      .unchunks

  @TestOnly // interrupt has not been tested!
  private[io] def readerToCharStream(reader: Reader, bufferSize: Int = DefaultBufferSize)
  : Stream[IO, Char] =
    Stream.suspend:
      val buffer = new Array[Char](bufferSize)
      Stream.repeatEval:
        virtualThread:
          reader.read(buffer)
        .handleCancel:
          virtualThread:
            // Don't interrupt a ReadFile, cancel may block.
            // Instead, we CloseFile while possibly in ReadFile.
            reader.close()
        .map:
          case -1 =>
            null
          case o if o < 1 =>
            throw new RuntimeException(s"'$reader'.read returned $o ?")
          case n =>
            Chunk.array(java.util.Arrays.copyOfRange(buffer, 0, n))
      .takeWhileNotNull
      .unchunks
