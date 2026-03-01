package js7.base.data

import java.util.Objects.{checkFromToIndex, checkIndex}
import js7.base.data.ByteSeqAsciiCharSequence.*
import js7.base.data.ByteSequence.ops.*

/** Interprets a ByteSeq as a CharSequence of ASCII characters.
  *
  * Non-ASCII characters (> \u007f) are replaced by a non-ASCII replacement character.
  */
final class ByteSeqAsciiCharSequence[ByteSeq: ByteSequence](
  private val byteSeq: ByteSeq,
  private val begin: Int,
  private val end: Int)
extends CharSequence:

  def this(byteSeq: ByteSeq) =
    this(byteSeq, 0, byteSeq.length)

  val length =
    end - begin

  override def hashCode(): Int =
    31 * 31 * byteSeq.hashCode + 31 * begin + end // Bit byteSeq.hashCode doesn't depend on content!!!

  override def equals(other: Any): Boolean =
    other match
      case other: ByteSeqAsciiCharSequence[_] =>
        byteSeq == other.byteSeq.asInstanceOf[ByteSeqAsciiCharSequence[ByteSeq]]
          && begin == other.begin
          && end == other.end

      case other: CharSequence =>
        var i = 0
        while i < length do
          if other.charAt(i) != charAt(i) then return false
          i += 1
        true

  def charAt(index: Int) =
    if index < 0 || index >= length then throw new IndexOutOfBoundsException
    toAscii(byteSeq(begin + index))

  def subSequence(begin: Int, end: Int): CharSequence =
    if begin == 0 && end == length then
      this
    else
      checkFromToIndex(begin, end, length)
      ByteSeqAsciiCharSequence(byteSeq, this.begin + begin, this.begin + end)

  def getChars(srcBegin: Int, srcEnd: Int, dst: Array[Char], dstBegin: Int) =
    checkFromToIndex(srcBegin, srcEnd, length)
    checkIndex(dstBegin, dst.length - (srcEnd - srcBegin) + 1)
    var s = begin + srcBegin
    val end = begin + srcEnd
    var d = dstBegin
    while s < end do
      dst(d) = toAscii(byteSeq(s))
      d += 1
      s += 1

  override def toString =
    val array = new Array[Char](length)
    getChars(0, length, array, 0)
    new String(array)


object ByteSeqAsciiCharSequence:

  private inline def toAscii(byte: Byte): Char =
    if byte < 0 then
      '\uFFFD'
    else
      byte.toChar
