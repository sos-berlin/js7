package js7.base.utils

import cats.Eq
import io.circe.Json
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.UTF_8
import js7.base.circeutils.CirceUtils._
import js7.base.data.{ByteArray, ByteSequence}
import js7.base.problem.Checked
import scodec.bits.ByteVector

final class ByteVectorByteSequence extends ByteSequence[ByteVector]
{
  override val typeName = "ByteVector"
  val empty = ByteVector.empty

  def fromArray(bytes: Array[Byte]) =
    if (bytes.isEmpty)
      empty
    else
      ByteVector(bytes)

  def fromByteArray(byteArray: ByteArray) =
    ByteVector.view(byteArray.unsafeArray)

  def unsafeWrap(bytes: Array[Byte]) =
    ByteVector.view(bytes)

  override def isEmpty(byteVector: ByteVector)(implicit ev: Eq[ByteVector]) =
    byteVector.isEmpty

  def eqv(a: ByteVector, b: ByteVector) =
    a === b

  def length(byteVector: ByteVector) =
    byteVector.length match {
      case len if len > Int.MaxValue => sys.error(s"ByteVector($len bytes) is to big")
      case len => len.toInt
    }

  def at(byteVector: ByteVector, i: Int) =
    byteVector(i)

  override def take(byteVector: ByteVector, n: Int): ByteVector =
    byteVector.take(n)

  override def drop(byteVector: ByteVector, n: Int): ByteVector =
    byteVector.drop(n)

  override def slice(byteVector: ByteVector, from: Int, until: Int): ByteVector =
    byteVector.slice(from, until)

  def iterator(byteVector: ByteVector) =
    byteVector.toIterable.iterator

  override def combine(a: ByteVector, b: ByteVector) =
    a ++ b

  //def checkedUtf8String(byteVector: ByteVector) =
  //  byteVector.decodeUtf8.toThrowableChecked

  def toArray(byteVector: ByteVector): Array[Byte] =
    byteVector.toArray

  //def copyToArray(byteVector: ByteVector, array: Array[Byte], start: Int, from: Int, length: Int): Unit =
  //  byteVector.copyToArray(array, start, from, length)

  override def toByteBuffer(byteVector: ByteVector): ByteBuffer =
    byteVector.toByteBuffer

  override def parseJson(byteVector: ByteVector): Checked[Json] =
    new String(byteVector.toArray, UTF_8).parseJsonChecked
}
