package js7.base.data

import cats.Eq
import io.circe.{Decoder, Json}
import java.io.{ByteArrayInputStream, InputStream, OutputStream}
import java.lang.System.arraycopy
import java.nio.charset.StandardCharsets.UTF_8
import java.util.Base64
import java.util.Objects.requireNonNull
import js7.base.circeutils.CirceUtils.*
import js7.base.data.ByteSequence.ops.*
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

  def iterator: Iterator[Byte] =
    unsafeArray.iterator

  def toInputStream: InputStream =
    new ByteArrayInputStream(unsafeArray)

  def toArray: Array[Byte] =
    java.util.Arrays.copyOf(unsafeArray, unsafeArray.length)

  def utf8String = new String(unsafeArray, UTF_8)

  def toMimeBase64: String =
    Base64.getMimeEncoder.encodeToString(unsafeArray)

  def parseJson: Checked[Json] =
    parseJsonByteArray(unsafeArray).toChecked

  def parseJsonAs[A: Decoder]: Checked[A] =
    parseJsonByteArray(unsafeArray).flatMap(_.as[A]).toChecked
    //utf8String.parseJsonAs[A]

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
  val clazz = classOf[ByteArray]

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

  override def toInputStream(byteArray: ByteArray): InputStream =
    byteArray.toInputStream

  def toArray(byteArray: ByteArray) =
    byteArray.toArray

  override def unsafeArray(byteArray: ByteArray) =
    byteArray.unsafeArray

  override def copyToArray(byteArray: ByteArray, array: Array[Byte]) =
    byteArray.copyToArray(array)

  override def copyToArray(byteArray: ByteArray, array: Array[Byte], start: Int, len: Int) =
    byteArray.copyToArray(array, start, len)

  override def slice(byteArray: ByteArray, from: Int, until: Int) =
    byteArray.slice(from, until)

  def combine(a: ByteArray, b: ByteArray): ByteArray =
    a ++ b

  override def combineAll(byteArraysOnce: IterableOnce[ByteArray]): ByteArray =
    combineByteSequences(byteArraysOnce)

  def combineByteSequences[ByteSeq](byteSeqsOnce: IterableOnce[ByteSeq])
    (implicit ByteSeq: ByteSequence[ByteSeq])
  : ByteArray =
    byteSeqsOnce.knownSize match {
      case 0 => ByteArray.empty
      case 1 => byteSeqsOnce.iterator.next().toByteArray
      case _ =>
        val byteArrays = mutable.ArrayBuffer.from(byteSeqsOnce)
        val result = new Array[Byte](byteArrays.view.map(_.length).sum)
        var pos = 0
        for (byteArray <- byteArrays) {
          byteArray.copyToArray(result, pos, byteArray.length)
          pos += byteArray.length
        }
        ByteArray.unsafeWrap(result)
    }

  override def writeToStream(byteArray: ByteArray, out: OutputStream) =
    byteArray.writeToStream(out)

  private[data] val inputStreamBufferSize = 32*1024

  override def fromInputStreamLimited(in: InputStream, limit: Int): Either[ByteArray, ByteArray] = {
    val buffer = mutable.Buffer.empty[ByteArray]
    var totalLength = 0
    var eof = false
    var overflow = false
    val limit1 = limit min Int.MaxValue - 1 // Avoids overflow when adding 1
    while (!eof && !overflow && totalLength <= limit) {
      val size1 = inputStreamBufferSize min limit1 - totalLength + 1
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
