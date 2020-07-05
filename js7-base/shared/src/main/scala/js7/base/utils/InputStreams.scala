package js7.base.utils

import java.io.InputStream
import scodec.bits.ByteVector

object InputStreams
{
  private[utils] val bufferSize = 32*1024

  def inputStreamToByteArray(in: InputStream): Array[Byte] =
    inputStreamToByteVector(in).toArray

  def inputStreamToByteArrayLimited(in: InputStream, limit: Long): Either[Array[Byte], Array[Byte]] =
    inputStreamToByteVectorLimited(in, limit) match {
      case Left(byteVector) => Left(byteVector.toArray)
      case Right(byteVector) => Right(byteVector.toArray)
    }

  def inputStreamToByteVector(in: InputStream): ByteVector =
    inputStreamToByteVectorLimited(in, Long.MaxValue)
      .getOrElse(throw new RuntimeException)

  def inputStreamToByteVectorLimited(in: InputStream, limit: Long): Either[ByteVector, ByteVector] = {
    var result = ByteVector.empty
    var eof = false
    var overflow = false
    val limit1 = limit min Long.MaxValue - 1 // Avoids overflow when adding 1
    while (!eof && !overflow && result.length <= limit) {
      val size1 = intMin(bufferSize, limit1 - result.length + 1)
      val bytes = new Array[Byte](size1)
      val readLength = in.read(bytes, 0, size1)
      eof = readLength <= 0
      if (!eof) {
        overflow = result.length + readLength > limit
        val length = intMin(readLength, limit - result.length)
        if (length == bytes.length) {
          result ++= ByteVector.view(bytes)
        } else
          result ++= ByteVector(bytes, 0, length)
      }
    }
    if (overflow) Left(result) else Right(result)
  }

  private def intMin(a: Int, b: Long): Int =
    if (a <= b) a
    else if (b <= Int.MaxValue) b.toInt
    else Int.MaxValue
}
