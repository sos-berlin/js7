package js7.base.io

import cats.effect.IO
import fs2.{Chunk, Stream}
import java.io.{InputStream, Reader}
import java.nio.ByteBuffer
import java.nio.channels.Channels.newChannel
import java.nio.channels.ReadableByteChannel
import js7.base.fs2utils.Fs2ChunkByteSequence
import js7.base.fs2utils.StreamExtensions.takeWhileNotNull
import js7.base.thread.IOExecutor.env.interruptibleVirtualThread

object ReaderStreams:

  private val DefaultBufferSize = 8192

  /** Returns a Chunk[Byte] immediately after each read operation. */
  def inputStreamToByteStream(in: InputStream, bufferSize: Int = DefaultBufferSize)
  : Stream[IO, Byte] =
    channelToByteStream(newChannel(in), bufferSize)

  def channelToByteStream(channel: ReadableByteChannel, bufferSize: Int = DefaultBufferSize)
  : Stream[IO, Byte] =
    Stream.suspend:
      val buffer = ByteBuffer.allocateDirect(bufferSize)
      Stream.repeatEval:
        interruptibleVirtualThread:
          buffer.clear()
          channel.read(buffer)
        .map:
          case -1 =>
            null
          case o if o < 1 =>
            throw new RuntimeException(s"'$channel'.read returned unexpected $o")
          case _ =>
            buffer.flip()
            Fs2ChunkByteSequence.readByteBuffer(buffer)
      .takeWhileNotNull
      .unchunks

  def readerToCharStream(reader: Reader, bufferSize: Int = DefaultBufferSize)
  : Stream[IO, Char] =
    Stream.suspend:
      val buffer = new Array[Char](bufferSize)
      Stream.repeatEval:
        interruptibleVirtualThread:
          reader.read(buffer)
        .map:
          case -1 =>
            null
          case o if o < 1 =>
            throw new RuntimeException(s"'$reader'.read returned $o ?")
          case n =>
            Chunk.array(java.util.Arrays.copyOfRange(buffer, 0, n))
      .takeWhileNotNull
      .unchunks
