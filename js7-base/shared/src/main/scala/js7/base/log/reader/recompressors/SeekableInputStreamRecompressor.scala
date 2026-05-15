package js7.base.log.reader.recompressors

import cats.effect.{IO, Resource}
import fs2.Chunk
import java.io.{InputStream, OutputStream}
import java.nio.charset.StandardCharsets.UTF_8
import js7.base.io.{SeekableInputStream, SeekableOutputStream}
import js7.base.log.reader.LogWriter
import js7.base.log.reader.recompressors.SeekableInputStreamRecompressor.*

trait SeekableInputStreamRecompressor extends Recompressor:

  private val dictionary = Dictionary

  protected def newCompressiongOutputStream(out: OutputStream): SeekableOutputStream

  def decompressingInputStream(in: InputStream): SeekableInputStream

  def toLogWriter(out: OutputStream): Resource[IO, LogWriter] =
    Resource.fromAutoCloseable:
      IO.blocking:
        newCompressiongOutputStream(out)
    .flatMap: (out: SeekableOutputStream) =>
      Resource.fromAutoCloseable:
        IO:
          new LogWriter with AutoCloseable:
            private val buf = new Array[Byte](512)
            private var _bytePosition = 0L

            def write(chunk: Chunk[Byte]): Unit =
              out.write(chunk.toArray)
              _bytePosition += chunk.size

            def position: Long =
              _bytePosition

            def markOpaquePos() =
              out.markOpaquePos()

            def close() =
              out.close()

object SeekableInputStreamRecompressor:
  private val Dictionary = "".getBytes(UTF_8) // not tested
