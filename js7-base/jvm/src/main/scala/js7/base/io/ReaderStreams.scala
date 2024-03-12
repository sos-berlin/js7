package js7.base.io

import cats.effect
import cats.effect.IO
import fs2.{Chunk, Stream}
import java.io.{InputStream, Reader}
import java.nio.ByteBuffer
import java.nio.channels.Channels.newChannel
import java.nio.channels.{Channels, ReadableByteChannel}
import java.util
import js7.base.data.ByteSequence
import js7.base.fs2utils.Fs2ChunkByteSequence.*
import js7.base.thread.IOExecutor

object ReaderStreams:

  private val DefaultBufferSize = 8192

  def inputStreamToByteStream(in: InputStream, bufferSize: Int = DefaultBufferSize)
    (using IOExecutor)
  : Stream[IO, Byte] =
    blockingChannelToByteStream(newChannel(in), bufferSize)

  def blockingChannelToByteStream(channel: ReadableByteChannel, bufferSize: Int = DefaultBufferSize)
    (using IOExecutor)
  : Stream[IO, Byte] =
    Stream.suspend:
      val buffer = ByteBuffer.allocateDirect(bufferSize)
      Stream.repeatEval:
        IOExecutor.interruptible:
          buffer.clear()
          channel.read(buffer)
        .map:
          case -1 =>
            null
          case o if o < 1 =>
            throw new RuntimeException(s"'$channel'.read return 0 ?'")
          case _ =>
            buffer.flip()
            ByteSequence[Chunk[Byte]].readByteBuffer(buffer)
      .takeWhile(_ != null)
      .unchunks

  def readerToCharStream(reader: Reader, bufferSize: Int = DefaultBufferSize)
    (using IOExecutor)
  : Stream[IO, Char] =
    Stream.suspend:
      val buffer = new Array[Char](bufferSize)
      Stream.repeatEval:
        IOExecutor.interruptible:
          reader.read(buffer)
        .map:
          case -1 =>
            null
          case o if o < 1 =>
            throw new RuntimeException(s"'$reader'.read returned $o ?'")
          case n =>
            Chunk.array(java.util.Arrays.copyOfRange(buffer, 0, n))
      .takeWhile(_ != null)
      .unchunks
