package js7.base.log.reader.recompressors

import java.io.{InputStream, OutputStream}
import java.nio.charset.StandardCharsets.UTF_8
import java.util.zip.Deflater
import js7.base.io.{DeflaterSeekableOutputStream, InflaterSeekableInputStream}
import js7.base.log.reader.LogIndexConf
import js7.base.utils.ScalaUtils.syntax.*

private case object DeflateRecompressor extends Recompressor:

  private val dictionary = "".getBytes(UTF_8) // not tested

  def findRecompressor(name: String) =
    (name == "deflate") ? this

  protected def newCompressiongOutputStream(out: OutputStream)(using LogIndexConf) =
    val deflater = new Deflater(Deflater.BEST_SPEED)
    deflater.setDictionary(dictionary)
    DeflaterSeekableOutputStream(out, deflater, bufferSize = 32*1024)

  def decompressingInputStream(in: InputStream)(using LogIndexConf) =
    InflaterSeekableInputStream(in, dictionary = dictionary, bufferSize = 4096/*guess*/)
