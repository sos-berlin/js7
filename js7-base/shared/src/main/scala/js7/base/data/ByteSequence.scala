package js7.base.data

import cats.effect.{Resource, SyncIO}
import cats.{Eq, Monoid, Show}
import io.circe.{Decoder, Json}
import java.io.{InputStream, OutputStream}
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.UTF_8
import java.util.Base64
import js7.base.circeutils.CirceUtils._
import js7.base.data.ByteSequence.{byteToPrintable, maxShowLength}
import js7.base.problem.{Checked, Problem}
import js7.base.utils.IOUtils
import js7.base.utils.ScalaUtils.syntax._
import scala.collection.immutable
import scala.language.implicitConversions
import scala.util.Random
import simulacrum._

@typeclass(excludeParents = List("Writable", "Monoid", "Eq", "Show"))
trait ByteSequence[ByteSeq] extends Writable[ByteSeq] with Monoid[ByteSeq] with Eq[ByteSeq] with Show[ByteSeq]
{
  implicit def implicitByteSequence: ByteSequence[ByteSeq] = this

  def typeName: String =
    getClass.simpleScalaName

  def apply[I](bytes: I*)(implicit I: Integral[I]): ByteSeq =
    unsafeWrap(bytes.view.map(i => I.toInt(i).toByte).toArray)

  def apply(array: Array[Byte]): ByteSeq =
    fromArray(array)

  def apply(string: String) =
    fromString(string)

  def fromArray(bytes: Array[Byte]): ByteSeq

  def fromArray(bytes: Array[Byte], from: Int, until: Int) =
    if (bytes.isEmpty)
      empty
    else
      unsafeWrap(bytes.slice(from, until))

  def fromByteArray(byteArray: ByteArray): ByteSeq

  def fromSeq(bytes: collection.Seq[Byte]): ByteSeq =
    bytes match {
      case arraySeq: immutable.ArraySeq.ofByte => unsafeWrap(arraySeq.unsafeArray)
      case _: Seq[Byte] => unsafeWrap(bytes.toArray)
      case _ => fromArray(bytes.toArray)  // This probably copies the array twice
    }

  def fromString(string: String): ByteSeq =
    unsafeWrap(string.getBytes(UTF_8))

  def fromMimeBase64(string: String): Checked[ByteSeq] =
    try Right(fromArray(Base64.getMimeDecoder.decode(string)))
    catch { case e: IllegalArgumentException =>
      Left(Problem(s"Invalid MIME base64 encoding: " + e.getMessage))
    }

  def fromInputStreamUnlimited(in: InputStream): ByteSeq =
    fromInputStreamLimited(in, Int.MaxValue)
      .getOrElse(throw new RuntimeException("fromInputStreamUnlimited")/*do not happen*/)

  def fromInputStreamLimited(in: InputStream, limit: Int): Either[ByteSeq, ByteSeq] =
    ByteArray.fromInputStreamLimited(in, limit) match {
      case Left(byteArray) => Left(fromByteArray(byteArray))
      case Right(byteArray) => Right(fromByteArray(byteArray))
    }

  def random(size: Int): ByteSeq = {
    val bytes = new Array[Byte](size)
    Random.nextBytes(bytes)
    unsafeWrap(bytes)
  }

  def unsafeWrap(bytes: Array[Byte]): ByteSeq

  def show(byteSeq: ByteSeq) = {
    val len = length(byteSeq)
    val prefix = take(byteSeq, maxShowLength)
    if (iterator(prefix).forall(o => o >= ' ' && o < '\u007f' || o == '\n' || o == '\r'))
      "»" + unsafeArray(byteSeq).map(byteToPrintable).mkString + "«" +
        (len > maxShowLength) ?? (s" ($len bytes)")
     else
      typeName + "(" +
        toStringAndHexRaw(prefix, maxShowLength, withEllipsis = len > maxShowLength) +
        (len > maxShowLength) ?? (s", $len bytes") +
        ")"
    }

  def toStringAndHexRaw(byteSeq: ByteSeq, n: Int = Int.MaxValue, withEllipsis: Boolean = false) =
    nonEmpty(byteSeq) ??
      ("»" +
        iterator(byteSeq).take(n).grouped(8).map(_.map(byteToPrintable).mkString).mkString +
        ((withEllipsis || n < length(byteSeq)) ?? "…") +
        "« " +
        toHexRaw(byteSeq, n, withEllipsis))

  def toHexRaw(byteSeq: ByteSeq, n: Int = Int.MaxValue, withEllipsis: Boolean = false): String =
    iterator(byteSeq).take(n).grouped(4).map(_.map(o => f"$o%02x").mkString).mkString(" ") +
      ((withEllipsis || n < length(byteSeq)) ?? "...")

  def nonEmpty(byteSeq: ByteSeq): Boolean =
    !isEmpty(byteSeq)


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

  def indexOf(byteSeq: ByteSeq, byte: Byte, from: Int): Int =
    indexOf(byteSeq, byte, from, Int.MaxValue)

  def indexOf(byteSeq: ByteSeq, byte: Byte, from: Int, until: Int): Int = {
    var i = from
    val u = until min length(byteSeq)
    while (i < u && at(byteSeq, i) != byte) i = i + 1
    if (i == u) -1 else i
  }

  def startsWith(byteSeq: ByteSeq, prefix: ByteSeq): Boolean = {
    val n = length(prefix)
    n <= length(byteSeq) &&
      (0 until n).forall(i => at(byteSeq, i) == at(prefix, i))
  }

  def endsWith(byteSeq: ByteSeq, suffix: ByteSeq): Boolean = {
    val n = length(suffix)
    val m = length(byteSeq) - n
    m >= 0 &&
      (0 until n).forall(i => at(byteSeq, m + i) == at(suffix, i))
  }

  def take(byteSeq: ByteSeq, n: Int) =
    slice(byteSeq, 0, n)

  def drop(byteSeq: ByteSeq, n: Int) =
    slice(byteSeq, n, length(byteSeq))

  def slice(byteSeq: ByteSeq, from: Int, until: Int): ByteSeq =
    if (from == 0 && until == length(byteSeq))
      byteSeq
    else
      unsafeWrap(unsafeArray(byteSeq).slice(from, until))

  def iterator(byteSeq: ByteSeq): Iterator[Byte]

  def utf8String(byteSeq: ByteSeq): String =
    new String(unsafeArray(byteSeq), UTF_8)

  def utf8StringTruncateAt(byteSeq: ByteSeq, truncateAt: Int): String =  // TODO Truncate big byte sequence before decoding
    utf8String(byteSeq).truncateWithEllipsis(truncateAt)

  def toArray(byteSeq: ByteSeq): Array[Byte]

  def unsafeArray(byteSeq: ByteSeq): Array[Byte] =
    toArray(byteSeq)

  //def copyToArray(byteSeq: ByteSeq, array: Array[Byte]): Int =
  //  copyToArray(byteSeq, array, 0, Int.MaxValue)
  //
  ///**
  //  * Fills the given `array` starting at index `start` with at most `len` bytes of this ByteSeq.
  //  *
  //  * Copying will stop once either all the elements of this ByteSeq have been copied,
  //  * or the end of the array is reached, or `len` elements have been copied.
  //  */
  //def copyToArray(byteSeq: ByteSeq, array: Array[Byte], start: Int, len: Int): Int =
  //  iterator(byteSeq).copyToArray(array, start, len)

  def toByteArray(byteSeq: ByteSeq): ByteArray =
    ByteArray.unsafeWrap(unsafeArray(byteSeq))

  def toByteBuffer(byteSeq: ByteSeq): ByteBuffer =
    ByteBuffer.wrap(unsafeArray(byteSeq)).asReadOnlyBuffer

  def toInputStream(byteSeq: ByteSeq): InputStream =
    new ByteSequenceInputStream(byteSeq)

  def toInputStreamResource(byteSeq: ByteSeq): Resource[SyncIO, InputStream] =
    Resource.fromAutoCloseable(SyncIO { toInputStream(byteSeq) })

  override def writeToStream(byteSeq: ByteSeq, out: OutputStream): Unit =
    IOUtils.copyStream(toInputStream(byteSeq), out)

  def parseJsonAs[B: Decoder](byteSeq: ByteSeq): Checked[B] =
    parseJson(byteSeq).flatMap(_.checkedAs[B])

  def parseJson(byteSeq: ByteSeq): Checked[Json] =
    parseJsonByteArray(unsafeArray(byteSeq)).toChecked
}

object ByteSequence
{
  private val maxShowLength = 32

  private def byteToPrintable(byte: Byte): Char =
    byte match {
      case byte if byte >= 0x20 && byte < 0x7f => byte.toChar
      case byte if byte >= 0 && byte < 0x20 => ('\u2400' + byte).toChar  // Control character representation
      case 0x7f => '\u2421'
      case _ => '�' // or ␦
    }
}
