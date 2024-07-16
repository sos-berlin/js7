package js7.base.data

import cats.effect.{Resource, SyncIO}
import cats.syntax.semigroup.*
import cats.{Eq, Monoid, Show}
import io.circe.{Decoder, Json}
import java.io.{FileInputStream, InputStream, OutputStream}
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Path
import java.util.Base64
import js7.base.circeutils.CirceUtils.*
import js7.base.data.ByteSequence.maxShowLength
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Ascii.byteToPrintableChar
import js7.base.utils.Assertions.assertThat
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Ascii, Assertions}
import scala.annotation.targetName
import scala.collection.immutable
import scala.collection.immutable.ArraySeq
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.util.Random

//@typeclass(excludeParents = List("Writable", "Monoid", "Eq", "Show"))
trait ByteSequence[ByteSeq]
extends Writable[ByteSeq], Monoid[ByteSeq], Eq[ByteSeq], Show[ByteSeq]:

  implicit def implicitByteSequence: ByteSequence[ByteSeq] = this

  def clazz: Class[ByteSeq]

  implicit lazy val classTag: ClassTag[ByteSeq] =
    ClassTag(clazz)

  final def typeName: String =
    clazz.simpleScalaName

  def apply[I](bytes: I*)(implicit I: Integral[I]): ByteSeq =
    unsafeWrap(bytes.view.map(i => I.toInt(i).toByte).toArray)

  def apply(array: Array[Byte]): ByteSeq =
    fromArray(array)

  def apply(string: String): ByteSeq =
    fromString(string)

  def one(byte: Byte): ByteSeq =
    unsafeWrap(Array(byte))

  def fromArray(bytes: Array[Byte]): ByteSeq

  def fromArray(bytes: Array[Byte], from: Int, until: Int): ByteSeq =
    if bytes.isEmpty then
      empty
    else
      unsafeWrap(bytes.slice(from, until))

  def fromByteArray(byteArray: ByteArray): ByteSeq

  def readByteBuffer(buffer: ByteBuffer): ByteSeq =
    val array = new Array[Byte](buffer.remaining)
    buffer.get(array)
    unsafeWrap(array)

  def fromSeq(bytes: collection.Seq[Byte]): ByteSeq =
    bytes match
      case arraySeq: immutable.ArraySeq.ofByte => unsafeWrap(arraySeq.unsafeArray)
      case _: Seq[Byte] => unsafeWrap(bytes.toArray)
      case _ => fromArray(bytes.toArray)  // This probably copies the array twice

  def fromString(string: String): ByteSeq =
    unsafeWrap(string.getBytes(UTF_8))

  def fromMimeBase64(string: String): Checked[ByteSeq] =
    try Right(fromArray(Base64.getMimeDecoder.decode(string)))
    catch { case e: IllegalArgumentException =>
      Left(Problem("Invalid MIME base64 encoding: " + e.getMessage))
    }

  def fromFileUnlimited(file: Path): ByteSeq =
    autoClosing(new FileInputStream(file.toFile))(in =>
      fromInputStreamUnlimited(in))

  def fromInputStreamUnlimited(in: InputStream): ByteSeq =
    fromInputStreamLimited(in, Int.MaxValue)
      .getOrElse(throw new RuntimeException("fromInputStreamUnlimited")/*doesn't happen*/)

  def fromInputStreamLimited(in: InputStream, limit: Int): Either[ByteSeq, ByteSeq] =
    ByteArray.fromInputStreamLimited(in, limit) match
      case Left(byteArray) => Left(fromByteArray(byteArray))
      case Right(byteArray) => Right(fromByteArray(byteArray))

  def random(size: Int): ByteSeq =
    val bytes = new Array[Byte](size)
    Random.nextBytes(bytes)
    unsafeWrap(bytes)

  def unsafeWrap(bytes: Array[Byte]): ByteSeq

  def wrapChunk(chunk: fs2.Chunk[Byte]): ByteSeq =
    unsafeWrap(chunk.toArray)

  def unsafeWrapChunk(chunk: fs2.Chunk[Byte]): ByteSeq =
    chunk match
      case fs2.Chunk.ArraySlice(array: Array[Byte] @unchecked, 0, len)
        if len == array.length =>
        unsafeWrap(array)
      case _ =>
        unsafeWrap(chunk.toArray)

  /** Fast only when ByteSeq wraps a single Array. */
  def toChunk(byteSeq: ByteSeq): fs2.Chunk[Byte] =
    fs2.Chunk.array(unsafeArray(byteSeq))

  def show(byteSeq: ByteSeq): String =
    show(byteSeq, maxShowLength)

  def show(byteSeq: ByteSeq, limit: Int): String =
    // TODO Test is missing
    val len = length(byteSeq)
    val prefix = take(byteSeq, limit)
    if iterator(prefix).forall(o => o >= ' ' && o < '\u007f' || o == '\n' || o == '\r') then
      "»" + unsafeArray(byteSeq).map(byteToPrintableChar).mkString + "«" +
        (len > limit) ?? (s" ($len bytes)")
    else
      typeName + "(" +
        toStringAndHexRaw(prefix, limit, withEllipsis = len > limit) +
        (len > limit) ?? (s", $len bytes") +
        ")"

  def toStringAndHexRaw(byteSeq: ByteSeq, n: Int = Int.MaxValue, withEllipsis: Boolean = false): String =
    nonEmpty(byteSeq) ??
      ("»" +
        iterator(byteSeq).take(n).grouped(8).map(_.map(byteToPrintableChar).mkString).mkString +
        ((withEllipsis || lengthIs(byteSeq) > n) ?? "…") +
        "« " +
        toHexRaw(byteSeq, n, withEllipsis))

  def toHexRaw(byteSeq: ByteSeq): String =
    toHexRaw(byteSeq, n = Int.MaxValue)

  def toHexRaw(byteSeq: ByteSeq, n: Int = Int.MaxValue, withEllipsis: Boolean = false): String =
    iterator(byteSeq).take(n).grouped(4).map(_.map(o => f"$o%02x").mkString).mkString(" ") +
      ((withEllipsis || lengthIs(byteSeq) > n) ?? "...")

  def toHex(byteSeq: ByteSeq): String =
    iterator(byteSeq).map(o => f"$o%02x").mkString

  def nonEmpty(byteSeq: ByteSeq): Boolean =
    !isEmpty(byteSeq)

  def intLength(byteSeq: ByteSeq): Int =
    length(byteSeq)

  def length(byteSeq: ByteSeq): Int

  def lengthIs(byteSeq: ByteSeq): Int =
    length(byteSeq)

  //@op("apply")
  def at(byteSeq: ByteSeq, i: Int): Byte

  def headOption(byteSeq: ByteSeq): Option[Byte] =
    nonEmpty(byteSeq) ? at(byteSeq, 0)

  def lastOption(byteSeq: ByteSeq): Option[Byte] =
    nonEmpty(byteSeq) ? at(byteSeq, length(byteSeq) - 1)

  def indexOf(byteSeq: ByteSeq, byte: Byte): Int =
    indexOf(byteSeq, byte, 0)

  def indexOf(byteSeq: ByteSeq, byte: Byte, from: Int): Int =
    indexOf(byteSeq, byte, from, Int.MaxValue)

  def indexOf(byteSeq: ByteSeq, byte: Byte, from: Int, until: Int): Int =
    var i = from
    val u = until min length(byteSeq)
    while i < u && at(byteSeq, i) != byte do i = i + 1
    if i == u then -1 else i

  def startsWith(byteSeq: ByteSeq, prefix: ByteSeq): Boolean =
    val n = length(prefix)
    lengthIs(byteSeq) >= n &&
      (0 until n).forall(i => at(byteSeq, i) == at(prefix, i))

  def endsWith(byteSeq: ByteSeq, suffix: ByteSeq): Boolean =
    val n = length(suffix)
    val m = length(byteSeq) - n
    m >= 0 &&
      (0 until n).forall(i => at(byteSeq, m + i) == at(suffix, i))

  def take(byteSeq: ByteSeq, n: Int): ByteSeq =
    slice(byteSeq, 0, n)

  def drop(byteSeq: ByteSeq, n: Int): ByteSeq =
    slice(byteSeq, n, length(byteSeq))

  def slice(byteSeq: ByteSeq, from: Int, until: Int): ByteSeq =
    if from <= 0 && lengthIs(byteSeq) <= until then
      byteSeq
    else
      unsafeWrap(unsafeArray(byteSeq).slice(from, until))

  @targetName("concat")
  def ++(start: ByteSeq, tail: ByteSeq): ByteSeq =
    start |+| tail

  //def chunk(byteSeq: ByteSeq, chunkSize: Int): Seq[ByteSeq] =
  //  length(byteSeq) match
  //    case 0 => Nil
  //    case 1 => byteSeq :: Nil
  //    case len =>
  //      val n = (len + chunkSize - 1) / chunkSize
  //      val array = new Array[ByteSeq](n)
  //      var i = 0
  //      while i < n do
  //        array(i) = slice(byteSeq, i * chunkSize, (i + 1) * chunkSize)
  //        i += 1
  //      ArraySeq.unsafeWrapArray(array)

  def chunkStream[F[_]](byteSeq: ByteSeq, maxSize: Int): fs2.Stream[F, ByteSeq] =
    assertThat(maxSize >= 1)
    length(byteSeq) match
      case 0 => fs2.Stream.empty
      case 1 => fs2.Stream.emit(byteSeq)
      case byteSeqLength =>
        fs2.Stream.unfold(0): offset =>
          val chunkLength = maxSize.min(byteSeqLength - offset)
          (chunkLength > 0) ?
            (this.slice(byteSeq, offset, until = offset + chunkLength), offset + chunkLength)

  def byteStream[F[_]](byteSeq: ByteSeq, chunkSize: Int): fs2.Stream[F, Byte] =
    length(byteSeq) match
      case 0 => fs2.Stream.empty
      case 1 => fs2.Stream.emit(at(byteSeq, 0))
      case _ =>
        unsafeWrappedArray(byteSeq) match
          case None =>
            chunkStream(byteSeq, chunkSize)
              .map(byteSeq => fs2.Chunk.array(unsafeArray(byteSeq)))
              .unchunks
          case Some(array) =>
            fs2.Stream.unfoldChunk(0): offset =>
              val chunkLength = chunkSize.min(array.length - offset)
              (chunkLength > 0) ?
                (fs2.Chunk.array(array, offset, chunkLength), offset + chunkLength)

  def iterator(byteSeq: ByteSeq): Iterator[Byte]

  def utf8String(byteSeq: ByteSeq): String =
    new String(unsafeArray(byteSeq), UTF_8)

  def utf8StringTruncateAt(byteSeq: ByteSeq, truncateAt: Int): String =  // TODO Truncate big byte sequence before decoding
    utf8String(byteSeq).truncateWithEllipsis(truncateAt)

  //def tryUtf8String(byteSeq: ByteSeq): Option[String] =
  //  try Some(UTF_8.newDecoder().decode(ByteBuffer.wrap(barr)))
  //  catch case _: CharacterCodingException => None

  def toArray(byteSeq: ByteSeq): Array[Byte]

  def unsafeArray(byteSeq: ByteSeq): Array[Byte] =
    unsafeWrappedArray(byteSeq) getOrElse toArray(byteSeq)

  def unsafeWrappedArray(byteSeq: ByteSeq): Option[Array[Byte]] =
    None

  def copyToArray(byteSeq: ByteSeq, array: Array[Byte]): Int =
    copyToArray(byteSeq, array, 0, Int.MaxValue)

  /**
    * Fills the given `array` starting at index `start` with at most `len` bytes of this ByteSeq.
    *
    * Copying will stop once either all the elements of this ByteSeq have been copied,
    * or the end of the array is reached, or `len` elements have been copied.
    */
  def copyToArray(byteSeq: ByteSeq, array: Array[Byte], start: Int, len: Int): Int =
    iterator(byteSeq).copyToArray(array, start, len)

  def toByteArray(byteSeq: ByteSeq): ByteArray =
    toByteSequence(byteSeq)(ByteArray)

  def toByteSequence[A](byteSeq: ByteSeq)(implicit A: ByteSequence[A]): A =
    if A.asInstanceOf[ByteSequence[ByteSeq]] eq this then
      byteSeq.asInstanceOf[A]
    else
      A.unsafeWrap(unsafeArray(byteSeq))

  def toByteBuffer(byteSeq: ByteSeq): ByteBuffer =
    ByteBuffer.wrap(unsafeArray(byteSeq)).asReadOnlyBuffer

  def toInputStream(byteSeq: ByteSeq): InputStream =
    new ByteSequenceInputStream(byteSeq)

  def toInputStreamResource(byteSeq: ByteSeq): Resource[SyncIO, InputStream] =
    Resource.fromAutoCloseable(SyncIO { toInputStream(byteSeq) })

  override def writeToStream(byteSeq: ByteSeq, out: OutputStream): Unit =
    toInputStream(byteSeq).transferTo(out)

  def parseJsonAs[B: Decoder](byteSeq: ByteSeq): Checked[B] =
    parseJson(byteSeq).flatMap(_.checkedAs[B])

  def parseJson(byteSeq: ByteSeq): Checked[Json] =
    parseJsonByteArray(unsafeArray(byteSeq)).toChecked
    // TODO Better performance with ByteBuffer instead of copying to an Array?
    //unsafeWrappedArray(byteSeq) match
    //  case None => parseJsonByteBuffer(toByteBuffer(byteSeq)).toChecked
    //  case Some(array) => parseJsonByteArray(array).toChecked


object ByteSequence:
  // Looks like generated by Simulacrum

  private val maxShowLength = 32

  def apply[A](implicit instance: ByteSequence[A]): ByteSequence[A] =
    instance

  trait Ops[ByteSeq]:
    def typeClassInstance: ByteSequence[ByteSeq]
    def self: ByteSeq

    def show: String =
      typeClassInstance.show(self)

    def show(limit: Int): String =
      typeClassInstance.show(self, limit = limit)

    def toStringAndHexRaw(n: Int = Int.MaxValue, withEllipsis: Boolean = false): String =
      typeClassInstance.toStringAndHexRaw(self, n, withEllipsis)

    def toHexRaw: String =
      typeClassInstance.toHexRaw(self)

    def toHexRaw(n: Int = Int.MaxValue, withEllipsis: Boolean = false): String =
      typeClassInstance.toHexRaw(self, n, withEllipsis)

    def toHex: String =
      typeClassInstance.toHex(self)

    def nonEmpty: Boolean =
      typeClassInstance.nonEmpty(self)

    def intLength: Int =
      typeClassInstance.intLength(self)

    def length: Int =
      typeClassInstance.length(self)

    def lengthIs: Int =
      typeClassInstance.lengthIs(self)

    def apply(i: Int): Byte =
      typeClassInstance.at(self, i)

    def headOption: Option[Byte] =
      typeClassInstance.headOption(self)

    def lastOption: Option[Byte] =
      typeClassInstance.lastOption(self)

    def indexOf(byte: Byte): Int =
      typeClassInstance.indexOf(self, byte)

    def indexOf(byte: Byte, from: Int): Int =
      typeClassInstance.indexOf(self, byte, from)

    def indexOf(byte: Byte, from: Int, until: Int): Int =
      typeClassInstance.indexOf(self, byte, from, until)

    def startsWith(prefix: ByteSeq): Boolean =
      typeClassInstance.startsWith(self, prefix)

    def endsWith(suffix: ByteSeq): Boolean =
      typeClassInstance.endsWith(self, suffix)

    def take(n: Int): ByteSeq =
      typeClassInstance.take(self, n)

    def drop(n: Int): ByteSeq =
      typeClassInstance.drop(self, n)

    def slice(from: Int, until: Int): ByteSeq =
      typeClassInstance.slice(self, from, until)

    @targetName("concat")
    def ++(tail: ByteSeq): ByteSeq =
      typeClassInstance.++(self, tail)

    /** Split ByteSeq to parts of chunkSize and stream them (by copying). */
    def chunkStream[F[_]](chunkSize: Int): fs2.Stream[F, ByteSeq] =
      typeClassInstance.chunkStream(self, chunkSize)

    /** Fast only when ByteSeq wraps a single Array. */
    def toChunk: fs2.Chunk[Byte] =
      typeClassInstance.toChunk(self)

    /** Tries not to copy the underlying Array. */
    def byteStream[F[_]](chunkSize: Int): fs2.Stream[F, Byte] =
      typeClassInstance.byteStream(self, chunkSize)

    def iterator: Iterator[Byte] =
      typeClassInstance.iterator(self)

    def utf8String: String =
      typeClassInstance.utf8String(self)

    def utf8StringTruncateAt(truncateAt: Int): String =
      typeClassInstance.utf8StringTruncateAt(self, truncateAt)

    def toArray: Array[Byte] =
      typeClassInstance.toArray(self)

    def unsafeArray: Array[Byte] =
      typeClassInstance.unsafeArray(self)

    /** Returns the underlying Array or None. */
    def unsafeWrappedArray: Option[Array[Byte]] =
      typeClassInstance.unsafeWrappedArray(self)

    def copyToArray(array: Array[Byte]): Int =
      typeClassInstance.copyToArray(self, array)

    /**
      * Fills the given `array` starting at index `start` with at most `len` bytes of this ByteSeq.
      *
      * Copying will stop once either all the elements of this ByteSeq have been copied,
      * or the end of the array is reached, or `len` elements have been copied.
      */
    def copyToArray(array: Array[Byte], start: Int, len: Int): Int =
      typeClassInstance.copyToArray(self, array, start, len)

    def toByteArray: ByteArray =
      typeClassInstance.toByteArray(self)

    def toByteSequence[A](implicit A: ByteSequence[A]): A =
      typeClassInstance.toByteSequence(self)

    def toByteBuffer: ByteBuffer =
      typeClassInstance.toByteBuffer(self)

    def toInputStream: InputStream =
      typeClassInstance.toInputStream(self)

    def toInputStreamResource: Resource[SyncIO, InputStream] =
      typeClassInstance.toInputStreamResource(self)

    def writeToStream(out: OutputStream): Unit =
      typeClassInstance.writeToStream(self, out)

    def parseJsonAs[B: Decoder]: Checked[B] =
      typeClassInstance.parseJsonAs(self)

    def parseJson: Checked[Json] =
      typeClassInstance.parseJson(self)

  trait ToByteSequenceOps:
    implicit def toByteSequenceOps[ByteSeq](target: ByteSeq)(implicit tc: ByteSequence[ByteSeq])
    : Ops[ByteSeq] =
      new Ops[ByteSeq]:
        val self = target
        val typeClassInstance = tc

  object nonInheritedOps extends ToByteSequenceOps

  trait AllOps[ByteSeq] extends Ops[ByteSeq]:
    def typeClassInstance: ByteSequence[ByteSeq]

  object ops:
    implicit def toAllByteSequenceOps[ByteSeq](target: ByteSeq)(implicit tc: ByteSequence[ByteSeq])
    : AllOps[ByteSeq] =
      new AllOps[ByteSeq]:
        val self = target
        val typeClassInstance = tc
