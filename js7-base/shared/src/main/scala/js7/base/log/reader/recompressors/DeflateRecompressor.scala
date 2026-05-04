package js7.base.log.reader.recompressors

import java.io.{InputStream, OutputStream}
import java.nio.charset.StandardCharsets.UTF_8
import java.util.zip.Deflater
import js7.base.io.{DeflaterSeekableOutputStream, InflaterSeekableInputStream, SeekableInputStream, SeekableOutputStream}
import js7.base.utils.ScalaUtils.syntax.*

private[reader] case object DeflateRecompressor extends SeekableInputStreamRecompressor:

  private val dictionary = "".getBytes(UTF_8) // not tested

  def findRecompressor(name: String) =
    (name == "deflate") ? this

  protected def newCompressiongOutputStream(out: OutputStream): SeekableOutputStream =
    val deflater = new Deflater(Deflater.BEST_SPEED)
    deflater.setDictionary(dictionary)
    DeflaterSeekableOutputStream(out, deflater, bufferSize = 32*1024)

  def decompressingInputStream(in: InputStream): SeekableInputStream =
    InflaterSeekableInputStream(in, dictionary = dictionary, bufferSize = 4096/*guess*/)
