package js7.base.log.reader.recompressors

import cats.effect.{IO, Resource}
import cats.syntax.option.none
import fs2.Chunk
import java.io.{InputStream, OutputStream}
import java.util.zip.{Deflater, GZIPInputStream, GZIPOutputStream}
import js7.base.io.{CountingOutputStream, OpaquePos}
import js7.base.log.reader.LogWriter
import js7.base.utils.ScalaUtils.syntax.*

// Slower than DeflateRecompressor
private[reader] case object GzipRecompressor extends Recompressor:

  def findRecompressor(name: String) =
    (name == "gzip") ? this

  def decompressingInputStream(in: InputStream): InputStream =
    new GZIPInputStream(in, 8192/*guess*/)

  def toLogWriter(out: OutputStream): Resource[IO, LogWriter] =
    Resource.fromAutoCloseable:
      IO:
        new LogWriter with AutoCloseable:
          private var _position = 0L
          private var _compressesPosition = 0L
          private var _gzip = none[MyGzipOutputStream]

          def write(chunk: Chunk[Byte]): Unit =
            val gzip = _gzip.getOrElse:
              val gzip = MyGzipOutputStream(out)
              _gzip = Some(gzip)
              gzip
            gzip.write(chunk.toArray)
            _position += chunk.size

          def position =
            _position

          def markOpaquePos() =
            _gzip.foreach: gzip =>
              _gzip = None
              gzip.close()
              _compressesPosition = _compressesPosition + gzip.byteCount
            OpaquePos(_compressesPosition)

          def close() =
            markOpaquePos()


  private final class MyGzipOutputStream private(out: CountingOutputStream)
  extends GZIPOutputStream(out, 8192):
    `def`.setLevel(Deflater.BEST_SPEED)

    def byteCount = out.byteCount

  private object MyGzipOutputStream:
    /** Doesn't close underlying OutputStream. */
    def apply(out: OutputStream): MyGzipOutputStream =
      new MyGzipOutputStream(
        CountingOutputStream(out, suppressClose = true))
