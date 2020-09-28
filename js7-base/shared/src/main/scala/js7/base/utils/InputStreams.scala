package js7.base.utils

import java.io.InputStream
import js7.base.data.ByteArray
import scala.collection.mutable

object InputStreams
{
  private[utils] val bufferSize = 32*1024

  def inputStreamToByteArray(in: InputStream): ByteArray =
    inputStreamToByteArrayLimited(in, Int.MaxValue)
      .getOrElse(throw new RuntimeException)

  def inputStreamToByteArrayLimited(in: InputStream, limit: Int): Either[ByteArray, ByteArray] = {
    var buffer = mutable.Buffer[ByteArray]()
    var totalLength = 0
    var eof = false
    var overflow = false
    val limit1 = limit min Int.MaxValue - 1 // Avoids overflow when adding 1
    while (!eof && !overflow && totalLength <= limit) {
      val size1 = bufferSize min limit1 - totalLength + 1
      val bytes = new Array[Byte](size1)
      val readLength = in.read(bytes, 0, size1)
      eof = readLength <= 0
      if (!eof) {
        overflow = totalLength + readLength > limit
        val length = readLength min limit - totalLength
        if (length == bytes.length) {
          buffer += ByteArray.unsafeWrap(bytes)
        } else
          buffer += ByteArray.fromArray(bytes, 0, length)
        totalLength += length
      }
    }
    val result = ByteArray.combineAll(buffer)
    if (overflow) Left(result) else Right(result)
  }
}
