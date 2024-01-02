package js7.base.fs2utils

import cats.effect.IO
import fs2.{Chunk, Stream}
import java.io.{InputStream, Reader}
import java.nio.ByteBuffer
import java.nio.channels.Channels.newChannel
import java.nio.channels.{Channels, ReadableByteChannel}
import java.util
import js7.base.utils.ScalaUtils.*

object StreamUtils:

  private val BufferSize = 8192

  def readerToCharStream(reader: Reader): Stream[IO, Char] =
    Stream.suspend:
      val buffer = new Array[Char](BufferSize)
      Stream
        .repeatEval:
          IO
            .interruptible:
              reader.read(buffer)
            .map:
              case -1 =>
                null
              case o if o < 1 =>
                throw new RuntimeException(s"'$reader'.read returned 0 ?'")
              case n =>
                Chunk.array(java.util.Arrays.copyOfRange(buffer, 0, n))
        .takeWhile(_ != null)
        .unchunks

  def inputStreamToByteStream(in: InputStream): Stream[IO, Byte] =
    blockingChannelToByteStream(newChannel(in))

  def blockingChannelToByteStream(channel: ReadableByteChannel): Stream[IO, Byte] =
    Stream.suspend:
      val buffer = ByteBuffer.allocateDirect(BufferSize)
      Stream
        .repeatEval:
          IO
            .interruptible:
              channel.read(buffer.clear())
            .map:
              case -1 =>
                null
              case o if o < 1 =>
                throw new RuntimeException(s"'$channel'.read return 0 ?'")
              case n =>
                val a = new Array[Byte](n)
                buffer.get(0, a)
                Chunk.array(a)
        .takeWhile(_ != null)
        .unchunks
