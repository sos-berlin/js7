package js7.common.akkautils

import akka.util.ByteString
import cats.Eq
import io.circe.Json
import java.nio.charset.StandardCharsets.UTF_8
import js7.base.circeutils.CirceUtils._
import js7.base.data.{ByteArray, ByteSequence}
import js7.base.problem.Checked

final class ByteStringByteSequence extends ByteSequence[ByteString]
{
  override val typeName = "ByteString"
  val empty = ByteString.empty

  override def fromArray(bytes: Array[Byte]) =
    ByteString(bytes)

  def fromByteArray(byteArray: ByteArray) =
    ByteString.fromArrayUnsafe(byteArray.unsafeArray)

  def unsafeWrap(bytes: Array[Byte]) =
    ByteString.fromArrayUnsafe(bytes)

  override def isEmpty(byteString: ByteString)(implicit ev: Eq[ByteString]) =
    byteString.isEmpty

  def eqv(a: ByteString, b: ByteString) =
    a == b

  def length(byteString: ByteString) =
    byteString.length

  def at(byteString: ByteString, i: Int) =
    byteString(i)

  override def take(byteString: ByteString, n: Int): ByteString =
    byteString.take(n)

  override def drop(byteString: ByteString, n: Int): ByteString =
    byteString.drop(n)

  override def slice(byteString: ByteString, from: Int, until: Int): ByteString =
    byteString.slice(from, until)

  def iterator(byteString: ByteString) =
    byteString.iterator

  def combine(a: ByteString, b: ByteString) =
    a ++ b

  def toArray(byteString: ByteString): Array[Byte] =
    byteString.toArray

  //def copyToArray(byteString: ByteString, array: Array[Byte], start: Int, length: Int) =
  //  byteString.copyToArray(array, start, length)

  override def parseJson(byteString: ByteString): Checked[Json] =
    byteString.decodeString(UTF_8).parseJson
}
