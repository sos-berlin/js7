package js7.base.data

import io.circe.{Decoder, Json}
import java.nio.charset.StandardCharsets.UTF_8
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Checked

final class ByteArray private(array: Array[Byte])
{
  def length = array.length

  def apply(i: Int) = array(i)

  def slice(from: Int, to: Int) = {
    if (from <= 0 && to >= array.length)
      this
    else
      new ByteArray(java.util.Arrays.copyOfRange(array, from max 0, to max length))
  }

  def ++(o: ByteArray) = {
    if (o.unsafeArray.isEmpty)
      this
    else if (array.isEmpty)
      o
    else {
      val a = new Array[Byte](length + o.length)
      System.arraycopy(array, 0, a, 0, array.length)
      System.arraycopy(o.unsafeArray, 0, a, array.length, o.length)
      ByteArray.unsafeWrap(a)
    }
  }

  def unsafeArray = array

  def utf8String = new String(array, UTF_8)

  def parseJson: Checked[Json] =
    parseJsonByteArray(array).toChecked

  def parseJsonAs[A: Decoder]: Checked[A] =
    parseJsonByteArray(array).flatMap(_.as[A]).toChecked
    //utf8String.parseJsonCheckedAs[A]

  override def equals(other: Any) =
    other match {
      case other: ByteArray => java.util.Arrays.equals(array, other.unsafeArray)
      case _ => false
    }

  override def hashCode = array.hashCode

  override def toString = {
    val prefix = array.take(32).grouped(4).toVector
    if (array.isEmpty)
      "ByteArray.empty"
    else
      "ByteArray(" +
        prefix.map(_.map(o => f"$o%02x").mkString).mkString(" ") + "  " +
        prefix.map(_.map(_.toChar).map(c => if (c >= ' ' && c < 0x7f) c else '¿').mkString).mkString(" ") +
        ")"
  }
}

object ByteArray extends ByteSequence[ByteArray]
{
  implicit val byteSequence: ByteSequence[ByteArray] = ByteArray

  val empty = new ByteArray(Array.empty)

  def eqv(a: ByteArray, b: ByteArray) =
    java.util.Arrays.equals(a.unsafeArray, b.unsafeArray)

  def fromArray(bytes: Array[Byte]) =
    if (bytes.isEmpty)
      empty
    else
      new ByteArray(bytes.clone())

  override def toByteArray(a: ByteArray) =
    a

  def unsafeWrap(bytes: Array[Byte]) =
    new ByteArray(bytes)

  def length(wrappedByteArray: ByteArray) =
    wrappedByteArray.length

  def at(wrappedByteArray: ByteArray, i: Int) =
    wrappedByteArray(i)

  override def indexOf(wrappedByteArray: ByteArray, byte: Byte, from: Int) =
    wrappedByteArray.unsafeArray.indexOf(byte, from)

  def toArray(wrappedByteArray: ByteArray) =
    java.util.Arrays.copyOf(wrappedByteArray.unsafeArray, wrappedByteArray.unsafeArray.length)

  override def unsafeArray(wrappedByteArray: ByteArray) =
    wrappedByteArray.unsafeArray

  def combine(a: ByteArray, b: ByteArray): ByteArray = {
    val array = new Array[Byte](length(a) + length(b))
    System.arraycopy(unsafeArray(a), 0, array, 0, length(a))
    System.arraycopy(unsafeArray(b), 0, array, length(a), length(b))
    unsafeWrap(array)
  }

  override def combineAll(byteArrays: IterableOnce[ByteArray]): ByteArray =
    byteArrays.knownSize match {
      case 0 => ByteArray.empty
      case 1 => byteArrays.iterator.next()
      case _ =>
        val byteArrays_ = byteArrays.iterator.to(Iterable)
        val array = new Array[Byte](byteArrays_.map(_.length).sum)
        var pos = 0
        for (byteArray <- byteArrays_) {
          System.arraycopy(byteArray.unsafeArray, 0, array, pos, byteArray.length)
          pos += byteArray.length
        }
        ByteArray(array)
    }
}
