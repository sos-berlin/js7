package js7.base.fs2utils

import cats.effect
import cats.effect.{IO, Resource}
import cats.syntax.flatMap.*
import fs2.{Chunk, Stream}
import java.io.{InputStream, Reader}
import java.nio.ByteBuffer
import java.nio.channels.Channels.newChannel
import java.nio.channels.{Channels, ReadableByteChannel}
import java.util
import js7.base.data.ByteSequence
import js7.base.fs2utils.Fs2ChunkByteSequence.*
import js7.base.log.Logger
import js7.base.system.Java8Polyfill.*
import js7.base.utils.CloseableIterator

object StreamUtils:

  private val DefaultBufferSize = 8192
  private val logger = Logger[this.type]

  def readerToCharStream(reader: Reader, bufferSize: Int = DefaultBufferSize): Stream[IO, Char] =
    Stream.suspend:
      val buffer = new Array[Char](bufferSize)
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

  def inputStreamToByteStream(in: InputStream, bufferSize: Int = DefaultBufferSize)
  : Stream[IO, Byte] =
    blockingChannelToByteStream(newChannel(in), bufferSize)

  def blockingChannelToByteStream(channel: ReadableByteChannel, bufferSize: Int = DefaultBufferSize)
  : Stream[IO, Byte] =
    Stream.suspend:
      val buffer = ByteBuffer.allocateDirect(bufferSize)
      Stream
        .repeatEval:
          IO
            .interruptible:
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

  def closeableIteratorToStream[A](iterator: CloseableIterator[A]): Stream[IO, A] =
    Stream
      .resource(Resource.makeCase(
        acquire = IO.pure(iterator))(
        release = (iterator, exitCase) => IO:
          logger.trace(s"Close $iterator $exitCase")
          iterator.close()))
      .flatMap: iterator =>
        Stream.fromIterator(iterator, chunkSize = 1/*???*/)

  /** Like Stream tailRecM, but limits the memory leak.
    * After a number of `Left` retured by `f`, the returned `Stream` is truncated.
    *
    * @see see Monix 3.2.1, https://github.com/monix/monix/issues/791
    */
  @deprecated
  def memoryLeakLimitedStreamTailRecM[A, B](a: A, limit: Int)(f: A => Stream[IO, Either[A, B]])
  : Stream[IO, B] =
    Stream.suspend:
      var leftCounter = 0
      a.tailRecM(a =>
        f(a).flatMap {
          case o @ Left(_) =>
            if leftCounter >= limit then {
              logger.debug(s"Limit Stream.tailRecM after $leftCounter× Left to reduce memory leakage")
              Stream.empty
            } else {
              leftCounter += 1
              Stream.emit(o)
            }

          case o => Stream.emit(o)
        })

  java8Polyfill();
