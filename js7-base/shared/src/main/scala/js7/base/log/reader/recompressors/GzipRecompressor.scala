package js7.base.log.reader.recompressors

import cats.effect.{IO, Resource}
import cats.syntax.option.none
import fs2.Chunk
import java.io.{InputStream, OutputStream}
import java.util.zip.{Deflater, GZIPInputStream, GZIPOutputStream}
import js7.base.io.CountingOutputStream
import js7.base.log.reader.LogFileIndex.LogWriter

private[reader] case object GzipRecompressor extends Recompressor:

  def decompressingInputStream(in: InputStream): InputStream =
    new GZIPInputStream(in, 4096/*guess*/)

  def toLogWriter(out: OutputStream): Resource[IO, LogWriter] =
    Resource.fromAutoCloseable:
      IO:
        new LogWriter with AutoCloseable:
          private var lastByteCount = 0L
          private var _gzip = none[MyGzipOutputStream]

          def write(chunk: Chunk[Byte]): Unit =
            val gzip = _gzip.getOrElse:
              val gzip = MyGzipOutputStream(out)
              _gzip = Some(gzip)
              gzip
            gzip.write(chunk.toArray)

          def markPosition() =
            _gzip.foreach: gzip =>
              _gzip = None
              gzip.close()
              lastByteCount += gzip.byteCount
            lastByteCount

          def close() =
            markPosition()


  private final class MyGzipOutputStream private(out: CountingOutputStream)
  extends GZIPOutputStream(out, 8192):
    `def`.setLevel(Deflater.BEST_SPEED)

    def byteCount = out.byteCount

  private object MyGzipOutputStream:
    /** Doesn't close underlying OutputStream. */
    def apply(out: OutputStream): MyGzipOutputStream =
      new MyGzipOutputStream(
        CountingOutputStream(out, suppressClose = true))
