package js7.base.data

import cats.effect.{Resource, SyncIO}
import cats.{Eq, Monoid}
import io.circe.{Decoder, Json}
import java.io.{InputStream, OutputStream}
import java.nio.charset.StandardCharsets.UTF_8
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Checked
import js7.base.utils.IOUtils
import js7.base.utils.ScalaUtils.syntax._
import scala.collection.immutable
import scala.language.implicitConversions
import scala.util.Random
import simulacrum._

@typeclass(excludeParents = "Writable" :: "Monoid" :: "Eq" :: Nil)
trait ByteSequence[ByteSeq] extends Writable[ByteSeq] with Monoid[ByteSeq] with Eq[ByteSeq]
{
  def apply[I](bytes: I*)(implicit I: Integral[I]): ByteSeq =
    unsafeWrap(bytes.view.map(i => I.toInt(i).toByte).toArray)

  def apply(array: Array[Byte]): ByteSeq =
    fromArray(array)

  def fromSeq(bytes: collection.Seq[Byte]): ByteSeq =
    bytes match {
      case arraySeq: immutable.ArraySeq.ofByte => unsafeWrap(arraySeq.unsafeArray)
      case _: immutable.Seq[Byte] => unsafeWrap(bytes.toArray)
      case _ => fromArray(bytes.toArray)  // This probably copies the array twice
    }

  def fromArray(bytes: Array[Byte]): ByteSeq

  final def apply(string: String) =
    fromString(string)

  def fromString(string: String) =
    unsafeWrap(string.getBytes(UTF_8))

  def random(size: Int): ByteSeq = {
    val bytes = new Array[Byte](size)
    Random.nextBytes(bytes)
    unsafeWrap(bytes)
  }

  def unsafeWrap(bytes: Array[Byte]): ByteSeq

  def nonEmpty(byteSeq: ByteSeq): Boolean =
    !isEmpty(byteSeq)(this)

  def intLength(byteSeq: ByteSeq): Int =
    length(byteSeq)

  def length(byteSeq: ByteSeq): Int

  @op("apply") def at(byteSeq: ByteSeq, i: Int): Byte

  def headOption(byteSeq: ByteSeq): Option[Byte] =
    nonEmpty(byteSeq) ? at(byteSeq, 0)

  def lastOption(byteSeq: ByteSeq): Option[Byte] =
    nonEmpty(byteSeq) ? at(byteSeq, length(byteSeq) - 1)

  def indexOf(byteSeq: ByteSeq, byte: Byte): Int =
    indexOf(byteSeq, byte, 0)

  def indexOf(byteSeq: ByteSeq, byte: Byte, from: Int): Int = {
    var i = from
    val length = this.length(byteSeq)
    while (i < length) {
      if (at(byteSeq, i) == byte) return i
      i += 1
    }
    -1
  }

  def drop(byteSeq: ByteSeq, n: Int) =
    slice(byteSeq, n, length(byteSeq))

  def slice(byteSeq: ByteSeq, from: Int, until: Int): ByteSeq =
    if (from == 0 && until == length(byteSeq))
      byteSeq
    else
      unsafeWrap(unsafeArray(byteSeq).slice(from, until))

  def utf8String(byteSeq: ByteSeq): String =
    new String(unsafeArray(byteSeq), UTF_8)

  def utf8StringTruncateAt(byteSeq: ByteSeq, truncateAt: Int): String =  // TODO Truncate big byte sequence before decoding
    utf8String(byteSeq).truncateWithEllipsis(truncateAt)

  def toArray(byteSeq: ByteSeq): Array[Byte]

  def unsafeArray(byteSeq: ByteSeq): Array[Byte] =
    toArray(byteSeq)

  def toByteArray(byteSeq: ByteSeq): ByteArray =
    ByteArray.unsafeWrap(unsafeArray(byteSeq))

  def toInputStream(byteSeq: ByteSeq): InputStream =
    new ByteSequenceInputStream(byteSeq)(this)

  def toInputStreamResource(byteSeq: ByteSeq): Resource[SyncIO, InputStream] =
    Resource.fromAutoCloseable(SyncIO { toInputStream(byteSeq) })

  override def writeToStream(byteSeq: ByteSeq, out: OutputStream): Unit =
    IOUtils.copyStream(toInputStream(byteSeq), out)

  def parseJsonAs[B: Decoder](byteSeq: ByteSeq): Checked[B] =
    parseJson(byteSeq).flatMap(_.checkedAs[B])

  def parseJson(byteSeq: ByteSeq): Checked[Json] =
    parseJsonByteArray(unsafeArray(byteSeq)).toChecked
}
