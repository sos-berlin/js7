package js7.base.log.reader.recompressors

import java.io.{BufferedInputStream, BufferedOutputStream, InputStream, OutputStream}
import js7.base.io.{OpaquePos, SeekableInputStream, SeekableOutputStream}
import js7.base.log.reader.LogIndexConf
import js7.base.utils.ScalaUtils.syntax.*

private case object PlainRecompressor extends Recompressor:

  override def isFast = true

  def findRecompressor(name: String) =
    (name == "plain") ? this

  def decompressingInputStream(in: InputStream)(using LogIndexConf) =
    SeekableInputStream(BufferedInputStream(in, 32 * 1024 /*guess*/))

  override protected def newCompressiongOutputStream(output: OutputStream)
    (using LogIndexConf)
  : SeekableOutputStream =
    new SeekableOutputStream(new BufferedOutputStream(output)):
      private var _position = 0L

      override def write(array: Array[Byte]): Unit =
        out.write(array)
        _position += array.length

      def position =
        _position

      def markOpaquePos() =
        OpaquePos(_position)
