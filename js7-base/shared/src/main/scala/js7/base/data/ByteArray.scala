package js7.base.data

import io.circe.{Decoder, Json}
import java.io.OutputStream
import java.nio.charset.StandardCharsets.UTF_8
import java.util.Base64
import java.util.Objects.requireNonNull
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Checked

final class ByteArray private(val unsafeArray: Array[Byte])
{
  def length = unsafeArray.length

  def apply(i: Int) = unsafeArray(i)

  def slice(from: Int, to: Int) = {
    if (from <= 0 && to >= unsafeArray.length)
      this
    else
      new ByteArray(java.util.Arrays.copyOfRange(unsafeArray, from max 0, to max length))
  }

  def ++(o: ByteArray) = {
    if (o.unsafeArray.isEmpty)
      this
    else if (unsafeArray.isEmpty)
      o
    else {
      val a = new Array[Byte](length + o.length)
      System.arraycopy(unsafeArray, 0, a, 0, unsafeArray.length)
      System.arraycopy(o.unsafeArray, 0, a, unsafeArray.length, o.length)
      ByteArray.unsafeWrap(a)
    }
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

  override def indexOf(byteArray: ByteArray, byte: Byte, from: Int) =
    byteArray.unsafeArray.indexOf(byte, from)

  def iterator(byteArray: ByteArray) =
    byteArray.unsafeArray.iterator

  def toArray(byteArray: ByteArray) =
    java.util.Arrays.copyOf(byteArray.unsafeArray, byteArray.unsafeArray.length)

  override def unsafeArray(byteArray: ByteArray) =
    byteArray.unsafeArray

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

  override def writeToStream(byteArray: ByteArray, out: OutputStream) =
    byteArray.writeToStream(out)
}
