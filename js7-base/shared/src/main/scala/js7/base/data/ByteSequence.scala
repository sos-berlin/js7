package js7.base.data

import cats.{Eq, Monoid}
import io.circe.{Decoder, Json}
import java.io.InputStream
import java.nio.charset.StandardCharsets.UTF_8
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax._
import scala.language.implicitConversions
import scala.util.Random
import simulacrum._

@typeclass(excludeParents = "Monoid" :: "Eq" :: Nil)
trait ByteSequence[ByteSeq] extends Monoid[ByteSeq] with Eq[ByteSeq]
{
  def apply[I](bytes: I*)(implicit I: Integral[I]): ByteSeq =
    unsafeWrap(bytes.view.map(i => I.toInt(i).toByte).toArray)

  def apply(array: Array[Byte]): ByteSeq =
    fromArray(array)

  def fromSeq(bytes: collection.Seq[Byte]): ByteSeq =
    fromArray(bytes.toArray)

  def fromArray(bytes: Array[Byte]): ByteSeq

  final def apply(string: String) =
    fromString(string)

  def fromString(string: String) =
    fromArray(string.getBytes(UTF_8))

  def random(size: Int): ByteSeq = {
    val bytes = new Array[Byte](size)
    Random.nextBytes(bytes)
    unsafeWrap(bytes)
  }

  def unsafeWrap(bytes: Array[Byte]): ByteSeq

  //def isEmpty(byteSequence: ByteSeq): Boolean =
  //  length(byteSequence) == 0
  //
  def nonEmpty(byteSequence: ByteSeq): Boolean =
    !isEmpty(byteSequence)(this)

  def intLength(byteSequence: ByteSeq): Int =
    length(byteSequence)

  def length(byteSequence: ByteSeq): Int

  @op("apply") def at(byteSequence: ByteSeq, i: Int): Byte

  def lastOption(byteSequence: ByteSeq): Option[Byte] =
    nonEmpty(byteSequence) ? at(byteSequence, length(byteSequence) - 1)

  def indexOf(byteSequence: ByteSeq, byte: Byte): Int =
    indexOf(byteSequence, byte, 0)

  def indexOf(byteSequence: ByteSeq, byte: Byte, from: Int): Int = {
    var i = from
    val length = this.length(byteSequence)
    while (i < length) {
      if (at(byteSequence, i) == byte) return i
      i += 1
    }
    -1
  }

  def drop(byteSequence: ByteSeq, n: Int) =
    slice(byteSequence, n, length(byteSequence))

  def slice(byteSequence: ByteSeq, from: Int, until: Int): ByteSeq =
    unsafeWrap(unsafeArray(byteSequence).slice(from, until))

  def utf8String(byteSequence: ByteSeq): String =
    new String(unsafeArray(byteSequence), UTF_8)

  def utf8StringTruncateAt(byteSequence: ByteSeq, truncateAt: Int): String =  // TODO Truncate big byte sequence before decoding
    utf8String(byteSequence).truncateWithEllipsis(truncateAt)

  def toArray(byteSequence: ByteSeq): Array[Byte]

  def unsafeArray(byteSequence: ByteSeq): Array[Byte] =
    toArray(byteSequence)

  def toInputStream(byteSequence: ByteSeq): InputStream =
    new ByteSequenceInputStream(byteSequence)(this)

  def parseJsonAs[B: Decoder](byteSequence: ByteSeq): Checked[B] =
    parseJson(byteSequence).flatMap(_.checkedAs[B])

  def parseJson(byteSequence: ByteSeq): Checked[Json] =
    parseJsonByteArray(unsafeArray(byteSequence)).toChecked
}
