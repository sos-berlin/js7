package js7.common.pekkoutils

import cats.Eq
import io.circe.Json
import java.nio.charset.StandardCharsets.UTF_8
import js7.base.circeutils.CirceUtils.*
import js7.base.data.{ByteArray, ByteSequence}
import js7.base.problem.Checked
import org.apache.pekko.util.{ByteString, CompactByteString}

object ByteStringByteSequence extends ByteSequence[ByteString]:

  val clazz: Class[ByteString] = classOf[ByteString]
  val empty: ByteString = ByteString.empty

  override def fromArray(bytes: Array[Byte]): ByteString =
    ByteString(bytes)

  def fromByteArray(byteArray: ByteArray): ByteString =
    ByteString.fromArrayUnsafe(byteArray.unsafeArray)

  def unsafeWrap(bytes: Array[Byte]): ByteString =
    ByteString.fromArrayUnsafe(bytes)

  override def isEmpty(byteString: ByteString)(implicit ev: Eq[ByteString]): Boolean =
    byteString.isEmpty

  def eqv(a: ByteString, b: ByteString): Boolean =
    a == b

  def length(byteString: ByteString): Int =
    byteString.length

  def at(byteString: ByteString, i: Int): Byte =
    byteString(i)

  override def take(byteString: ByteString, n: Int): ByteString =
    byteString.take(n)

  override def drop(byteString: ByteString, n: Int): ByteString =
    byteString.drop(n)

  override def slice(byteString: ByteString, from: Int, until: Int): ByteString =
    byteString.slice(from, until)

  def iterator(byteString: ByteString): Iterator[Byte] =
    byteString.iterator

  def combine(a: ByteString, b: ByteString): ByteString =
    a ++ b

  override def combineAll(as: IterableOnce[ByteString]): ByteString =
    val b = ByteString.newBuilder
    for a <- as.iterator do b.append(a)
    b.result()

  def toArray(byteString: ByteString): Array[Byte] =
    byteString.toArray

  override def unsafeArray(byteString: ByteString): Array[Byte] =
    byteString.toArrayUnsafe()

  override def unsafeWrappedArray(byteString: ByteString): Option[Array[Byte]] =
    byteString match
      case byteString: CompactByteString => Some(byteString.toArrayUnsafe())
      //Inaccessible: case ByteString1(array, 0, len) if len == array.length => Some(array)
      case _ => None

  override def copyToArray(byteString: ByteString, array: Array[Byte]): Int =
    byteString.copyToArray(array)

  override def copyToArray(byteString: ByteString, array: Array[Byte], start: Int, len: Int): Int =
    byteString.copyToArray(array, start, len)

  //def copyToArray(byteString: ByteString, array: Array[Byte], start: Int, length: Int) =
  //  byteString.copyToArray(array, start, length)

  override def parseJson(byteString: ByteString): Checked[Json] =
    byteString.decodeString(UTF_8).parseJson
