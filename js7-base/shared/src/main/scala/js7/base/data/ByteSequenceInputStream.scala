package js7.base.data

import java.io.InputStream

/** Slow if ByteSeq has many Chunks. */
final class ByteSequenceInputStream[A](byteSeq: A)(implicit A: ByteSequence[A]) extends InputStream:

  private[this] var i = 0
  private[this] var marked = 0
  private val length = A.length(byteSeq)

  def read(): Int =
    if i == length then
      -1
    else
      i += 1
      A.at(byteSeq, i - 1) & 0xff

  //override def read(array: Array[Byte], offset: Int, len: Int) =
  //  if (i == length)
  //    -1
  //  else {
  //    // SLOW: ByteSeq.apply iterates through all Chunks until the index is found !
  //    val readLength = len min length - i min Int.MaxValue
  //    byteSeq.copyToArray(array, offset, i, readLength)
  //    i += readLength
  //    readLength
  //  }

  override def markSupported = true

  override def mark(readlimit: Int): Unit =
    marked = i

  override def reset(): Unit =
    i = marked
