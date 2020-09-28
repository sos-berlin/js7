package js7.base.data

import cats.Eq
import io.circe.{Decoder, Json}
import java.io.OutputStream
import java.lang.System.arraycopy
import java.nio.charset.StandardCharsets.UTF_8
import java.util.Base64
import java.util.Objects.requireNonNull
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Checked
import scala.collection.mutable

final class ByteArray private(val unsafeArray: Array[Byte])
{
  def length = unsafeArray.length

  def apply(i: Int) = unsafeArray(i)

  def isEmpty = unsafeArray.isEmpty

  def copyToArray(array: Array[Byte]): Int =
    copyToArray(array, 0, Int.MaxValue)

  def copyToArray(array: Array[Byte], start: Int, len: Int): Int = {
    val n = len min unsafeArray.length min array.length - start
    arraycopy(unsafeArray, 0, array, start, n)
    n
  }

  def slice(from: Int, until: Int) =
    if (from >= until)
      ByteArray.empty
    else if (from <= 0 && until >= unsafeArray.length)
      this
    else
      new ByteArray(java.util.Arrays.copyOfRange(unsafeArray, from max 0, until min length))

  def ++(o: ByteArray) =
    if (o.isEmpty) this
    else if (isEmpty) o
    else {
      val a = new Array[Byte](length + o.length)
      arraycopy(unsafeArray, 0, a, 0, unsafeArray.length)
      arraycopy(o.unsafeArray, 0, a, unsafeArray.length, o.length)
      ByteArray.unsafeWrap(a)
    }

  def utf8String = new String(unsafeArray, UTF_8)

  def toMimeBase64: String =
    Base64.getMimeEncoder.encodeToString(unsafeArray)

  def parseJson: Checked[Json] =
    parseJsonByteArray(unsafeArray).toChecked

  def parseJsonAs[A: Decoder]: Checked[A] =
    parseJsonByteArray(unsafeArray).flatMap(_.as[A]).toChecked
    //utf8String.parseJsonCheckedAs[A]

  def writeToStream(out: OutputStream): Unit =
    out.write(unsafeArray)

  override def equals(other: Any) =
    other match {
      case other: ByteArray => java.util.Arrays.equals(unsafeArray, other.unsafeArray)
      case _ => false
    }

  override def hashCode = unsafeArray.hashCode

  override def toString = ByteArray.show(this)
}

object ByteArray extends ByteSequence[ByteArray]
{
  val empty = new ByteArray(Array.empty)

  // Hide IntelliJ red underlines here
  override def apply(string: String): ByteArray =
    super.apply(string)

  override def isEmpty(byteArray: ByteArray)(implicit ev: Eq[ByteArray]) =
    byteArray.isEmpty

  def eqv(a: ByteArray, b: ByteArray) =
    java.util.Arrays.equals(a.unsafeArray, b.unsafeArray)

  def fromArray(bytes: Array[Byte]) =
    if (bytes.isEmpty)
      empty
    else
      new ByteArray(bytes.clone())

  def fromByteArray(byteArray: ByteArray) =
    byteArray

  override def toByteArray(a: ByteArray) =
    a

  def unsafeWrap(bytes: Array[Byte]) =
    new ByteArray(requireNonNull(bytes))

  def length(byteArray: ByteArray) =
    byteArray.length

  def at(byteArray: ByteArray, i: Int) =
    byteArray(i)

  def iterator(byteArray: ByteArray) =
    byteArray.unsafeArray.iterator

  def toArray(byteArray: ByteArray) =
    java.util.Arrays.copyOf(byteArray.unsafeArray, byteArray.unsafeArray.length)

  override def unsafeArray(byteArray: ByteArray) =
    byteArray.unsafeArray

  //def copyToArray(byteArray: ByteArray, array: Array[Byte]) =
  //  byteArray.copyToArray(array)
  //
  //def copyToArray(byteArray: ByteArray, array: Array[Byte], start: Int, len: Int) =
  //  byteArray.copyToArray(array, start, len)

  override def slice(byteArray: ByteArray, from: Int, until: Int) =
    byteArray.slice(from, until)

  def combine(a: ByteArray, b: ByteArray): ByteArray = {
    val array = new Array[Byte](length(a) + length(b))
    arraycopy(unsafeArray(a), 0, array, 0, length(a))
    arraycopy(unsafeArray(b), 0, array, length(a), length(b))
    unsafeWrap(array)
  }

  override def combineAll(byteArraysOnce: IterableOnce[ByteArray]): ByteArray =
    byteArraysOnce.knownSize match {
      case 0 => ByteArray.empty
      case 1 => byteArraysOnce.iterator.next()
      case _ =>
        val byteArrays = mutable.ArrayBuffer.from(byteArraysOnce)
        val result = new Array[Byte](byteArrays.view.map(_.length).sum)
        var pos = 0
        for (byteArray <- byteArrays) {
          arraycopy(byteArray.unsafeArray, 0, result, pos, byteArray.length)
          pos += byteArray.length
        }
        ByteArray.unsafeWrap(result)
    }

  override def writeToStream(byteArray: ByteArray, out: OutputStream) =
    byteArray.writeToStream(out)
}
