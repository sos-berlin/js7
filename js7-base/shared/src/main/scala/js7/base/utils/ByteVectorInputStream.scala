package js7.base.utils

import java.io.InputStream
import scodec.bits.ByteVector

/** Slow if ByteVector has many Chunks. */
final class ByteVectorInputStream(byteVector: ByteVector) extends InputStream
{
  private[this] var i = 0
  private[this] var marked = 0

  def read() = {
    if (i == byteVector.length)
      -1
    else {
      // SLOW: ByteVector.apply iterates through all Chunks until the index is found !!!
      val byte = byteVector(i)
      i += 1
      byte & 0xFF
    }
  }

  //Test required:
  //override def read(array: Array[Byte], offset: Int, length: Int) = {
  //  if (i == byteVector.length)
  //    -1
  //  else {
  //    // SLOW: ByteVector.apply iterates through all Chunks until the index is found !
  //    val readLength = length min (byteVector.length - i min Int.MaxValue).toInt
  //    byteVector.copyToArray(array, offset, i, readLength)
  //    i += readLength
  //    readLength
  //  }
  //}

  override def markSupported = true

  override def mark(readlimit: Int) =
    marked = i

  override def reset() =
    i = marked
}
