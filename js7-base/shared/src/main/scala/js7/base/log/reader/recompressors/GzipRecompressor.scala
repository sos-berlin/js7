package js7.base.log.reader.recompressors

import cats.syntax.option.none
import java.io.{InputStream, OutputStream}
import java.util.zip.{Deflater, GZIPInputStream, GZIPOutputStream}
import js7.base.io.{CountingOutputStream, OpaquePos, SeekableInputStream, SeekableOutputStream}
import js7.base.log.reader.LogIndexConf
import js7.base.utils.ScalaUtils.syntax.*

// Slower than DeflateRecompressor — old code, use DeflateRecompressor!
@deprecated("DON'T USE, RecompressorTest FAILS")
private case object GzipRecompressor extends Recompressor:

  def findRecompressor(name: String) =
    (name == "gzip") ? this

  def decompressingInputStream(in: InputStream)(using conf: LogIndexConf) =
    SeekableInputStream:
      GZIPInputStream(in, conf.fileBufferSize / 8/*compression ratio*/)

  protected def newCompressiongOutputStream(output: OutputStream)(using LogIndexConf) =
    new SeekableOutputStream(output):
      private var _position = 0L
      private var _compressesPosition = 0L
      private var _gzip = none[MyGzipOutputStream]

      override def write(array: Array[Byte]): Unit =
        val gzip = _gzip.getOrElse:
          val gzip = MyGzipOutputStream(out)
          _gzip = Some(gzip)
          gzip
        gzip.write(array)
        _position += array.length

      def position =
        _position

      def markOpaquePos() =
        _gzip.foreach: gzip =>
          _gzip = None
          gzip.close()
          _compressesPosition = _compressesPosition + gzip.byteCount
        OpaquePos(_compressesPosition)

      override def close() =
        markOpaquePos()
        super.close()


  private final class MyGzipOutputStream private(out: CountingOutputStream)
  extends GZIPOutputStream(out, 8192):
    `def`.setLevel(Deflater.BEST_SPEED)

    def byteCount = out.byteCount

  private object MyGzipOutputStream:
    /** Doesn't close underlying OutputStream. */
    def apply(out: OutputStream): MyGzipOutputStream =
      new MyGzipOutputStream(
        CountingOutputStream(out, suppressClose = true))
