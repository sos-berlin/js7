package js7.base.data

import java.io.InputStream

/** Slow if ByteSeq has many Chunks. */
final class ByteSequenceInputStream[A](byteSeq: A)(implicit A: ByteSequence[A]) extends InputStream
{
  private[this] var i = 0
  private[this] var marked = 0

  def read() = {
    if (i == A.length(byteSeq))
      -1
    else {
      // SLOW: ByteSeq.apply iterates through all Chunks until the index is found !!!
      val byte = A.at(byteSeq, i)
      i += 1
      byte & 0xFF
    }
  }

  //Test required:
  //override def read(array: Array[Byte], offset: Int, length: Int) = {
  //  if (i == byteSeq.length)
  //    -1
  //  else {
  //    // SLOW: ByteSeq.apply iterates through all Chunks until the index is found !
  //    val readLength = length min (byteSeq.length - i min Int.MaxValue).toInt
  //    byteSeq.copyToArray(array, offset, i, readLength)
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
