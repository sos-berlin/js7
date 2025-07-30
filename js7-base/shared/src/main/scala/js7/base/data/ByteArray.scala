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
import scala.annotation.unused
import scala.collection.mutable

final class ByteArray private(val unsafeArray: Array[Byte]):

  def length: Int = unsafeArray.length

  def apply(i: Int): Byte = unsafeArray(i)

  def isEmpty: Boolean = unsafeArray.isEmpty

  def copyToArray(array: Array[Byte]): Int =
    copyToArray(array, 0, Int.MaxValue)

  def copyToArray(array: Array[Byte], start: Int, len: Int): Int =
    val n = len min unsafeArray.length min array.length - start
    arraycopy(unsafeArray, 0, array, start, n)
    n

  def slice(from: Int, until: Int): ByteArray =
    if from >= until then
      ByteArray.empty
    else if from <= 0 && until >= unsafeArray.length then
      this
    else
      new ByteArray(java.util.Arrays.copyOfRange(unsafeArray, from max 0, until min length))

  def ++(o: ByteArray): ByteArray =
    if o.isEmpty then this
    else if isEmpty then o
    else
      val a = new Array[Byte](length + o.length)
      arraycopy(unsafeArray, 0, a, 0, unsafeArray.length)
      arraycopy(o.unsafeArray, 0, a, unsafeArray.length, o.length)
      ByteArray.unsafeWrap(a)

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

  override def equals(other: Any): Boolean =
    other match
      case other: ByteArray => java.util.Arrays.equals(unsafeArray, other.unsafeArray)
      case _ => false

  override def hashCode: Int = unsafeArray.hashCode

  override def toString: String = ByteArray.show(this)


object ByteArray extends ByteSequence[ByteArray]:
  val clazz: Class[ByteArray] = classOf[ByteArray]

  val empty = new ByteArray(Array.empty)

  // Hide IntelliJ red underlines here
  override def apply(string: String): ByteArray =
    super.apply(string)

  override def isEmpty(byteArray: ByteArray)(using @unused Eq: Eq[ByteArray]): Boolean =
    byteArray.isEmpty

  def eqv(a: ByteArray, b: ByteArray): Boolean =
    java.util.Arrays.equals(a.unsafeArray, b.unsafeArray)

  def fromArray(bytes: Array[Byte]): ByteArray =
    if bytes.isEmpty then
      empty
    else
      new ByteArray(bytes.clone())

  def fromByteArray(byteArray: ByteArray): ByteArray =
    byteArray

  override def toByteArray(a: ByteArray): ByteArray =
    a

  def unsafeWrap(bytes: Array[Byte]) =
    new ByteArray(requireNonNull(bytes))

  // TODO Implement ByteArray for slices ?
  /** Emulated for ByteArray — ByteArray doesn't wrap slices. */
  def unsafeWrap(bytes: Array[Byte], offset: Int, length: Int): ByteArray =
    if offset == 0 && length == bytes.length then
      unsafeWrap(bytes)
    else
      unsafeWrap(bytes.slice(offset, offset + length))

  def length(byteArray: ByteArray): Int =
    byteArray.length

  def at(byteArray: ByteArray, i: Int): Byte =
    byteArray(i)

  def iterator(byteArray: ByteArray): Iterator[Byte] =
    byteArray.unsafeArray.iterator

  override def toInputStream(byteArray: ByteArray): InputStream =
    byteArray.toInputStream

  def toArray(byteArray: ByteArray): Array[Byte] =
    byteArray.toArray

  override def unsafeArray(byteArray: ByteArray): Array[Byte] =
    byteArray.unsafeArray

  override def unsafeWrappedArray(byteArray: ByteArray): Option[Array[Byte]] =
    Some(byteArray.unsafeArray)

  override def copyToArray(byteArray: ByteArray, array: Array[Byte]): Int =
    byteArray.copyToArray(array)

  override def copyToArray(byteArray: ByteArray, array: Array[Byte], start: Int, len: Int): Int =
    byteArray.copyToArray(array, start, len)

  override def slice(byteArray: ByteArray, from: Int, until: Int): ByteArray =
    byteArray.slice(from, until)

  def combine(a: ByteArray, b: ByteArray): ByteArray =
    a ++ b

  override def combineAll(byteArraysOnce: IterableOnce[ByteArray]): ByteArray =
    combineByteSequences(byteArraysOnce)

  def combineByteSequences[ByteSeq](byteSeqsOnce: IterableOnce[ByteSeq])
    (implicit ByteSeq: ByteSequence[ByteSeq])
  : ByteArray =
    byteSeqsOnce.knownSize match
      case 0 => ByteArray.empty
      case 1 => byteSeqsOnce.iterator.next().toByteArray
      case _ =>
        val byteArrays = mutable.ArrayBuffer.from(byteSeqsOnce)
        val result = new Array[Byte](byteArrays.view.map(_.length).sum)
        var pos = 0
        for byteArray <- byteArrays do
          byteArray.copyToArray(result, pos, byteArray.length)
          pos += byteArray.length
        ByteArray.unsafeWrap(result)

  override def writeToStream(byteArray: ByteArray, out: OutputStream): Unit =
    byteArray.writeToStream(out)

  private[data] val inputStreamBufferSize = 32*1024

  override def fromInputStreamLimited(in: InputStream, limit: Int): Either[ByteArray, ByteArray] =
    val buffer = mutable.Buffer.empty[ByteArray]
    var totalLength = 0
    var eof = false
    var overflow = false
    val limit1 = limit min Int.MaxValue - 1 // Avoids overflow when adding 1
    while !eof && !overflow && totalLength <= limit do
      val size1 = inputStreamBufferSize min limit1 - totalLength + 1
      val bytes = new Array[Byte](size1)
      val readLength = in.read(bytes, 0, size1)
      eof = readLength <= 0
      if !eof then
        overflow = totalLength + readLength > limit
        val length = readLength min limit - totalLength
        if length == bytes.length then
          buffer += ByteArray.unsafeWrap(bytes)
        else
          buffer += ByteArray.fromArray(bytes, 0, length)
        totalLength += length
    val result = ByteArray.combineAll(buffer)
    if overflow then Left(result) else Right(result)
