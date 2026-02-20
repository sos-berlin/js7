package js7.base.fs2utils

import cats.Eq
import fs2.Chunk
import java.nio.ByteBuffer
import js7.base.data.ByteSequence.nonInheritedOps.toByteSequenceOps
import js7.base.data.{ByteArray, ByteSequence}
import js7.base.utils.JavaVectors.vectorIndexOf
import scala.annotation.unused

object Fs2ChunkByteSequence extends ByteSequence[Chunk[Byte]]:

  val clazz: Class[Chunk[Byte]] = classOf[Chunk[Byte]]
  val empty: Chunk[Byte] = Chunk.empty

  override def one(byte: Byte): Chunk[Byte] =
    Chunk.singleton(byte)

  def fromArray(bytes: Array[Byte]): Chunk[Byte] =
    Chunk.array(bytes.clone())

  def fromByteArray(byteArray: ByteArray): Chunk[Byte] =
    Chunk.array(byteArray.unsafeArray)

  def unsafeWrap(bytes: Array[Byte]): Chunk[Byte] =
    Chunk.array(bytes)

  def unsafeWrap(bytes: Array[Byte], offset: Int, length: Int): Chunk[Byte] =
    Chunk.array(bytes, offset, length)

  def newBuilder(sizeHint: Int): Builder =
    new Builder:
      private var builder = Chunk.newBuilder[Byte]
      private var _isEmpty = true

      def isEmpty =
        _isEmpty

      def append(chunk: Chunk[Byte]) =
        if chunk.nonEmpty then
          builder += chunk
          _isEmpty = false
        this

      def clear() =
        builder = Chunk.newBuilder[Byte]
        this

      def result() =
        builder.result

  override def isEmpty(chunk: Chunk[Byte])(using @unused Eq: Eq[Chunk[Byte]]): Boolean =
    chunk.isEmpty

  override def indexOf(chunk: Chunk[Byte], byte: Byte, from: Int, until: Int): Int =
    chunk match
      case chunk: Chunk.ArraySlice[Byte] =>
        if from < 0 then
          throw IndexOutOfBoundsException(s"ByteSeq[fs2.Chunk].indexOf($byte, $from, $until)")
        val from_ = from min chunk.length
        val until_ = until min chunk.length
        val index = chunk.values.vectorIndexOf(byte, chunk.offset + from_, chunk.offset + until_)
        assert(index == -1 || index >= chunk.offset && index < chunk.offset + chunk.length) // TODO delete this
        if index < 0 then index else index - chunk.offset

      case chunk: Chunk.Singleton[Byte] =>
        if from == 0 && until > 0 && chunk.value == byte then
          from
        else
          -1

      case _ =>
        super.indexOf(chunk, byte, from, until)

  def length(chunk: Chunk[Byte]): Int =
    chunk.size

  def at(chunk: Chunk[Byte], i: Int): Byte =
    chunk(i)

  override def take(chunk: Chunk[Byte], n: Int): Chunk[Byte] =
    chunk.take(n)

  override def drop(chunk: Chunk[Byte], n: Int): Chunk[Byte] =
    chunk.drop(n)

  override def slice(chunk: Chunk[Byte], from: Int, until: Int): Chunk[Byte] =
    chunk.drop(from).take(until - from)

  def iterator(chunk: Chunk[Byte]): Iterator[Byte] =
    chunk.iterator

  def combine(a: Chunk[Byte], b: Chunk[Byte]): Chunk[Byte] =
    a ++ b

  override def combineAll(as: IterableOnce[Chunk[Byte]]): Chunk[Byte] =
    val b = Chunk.newBuilder[Byte]
    for a <- as.iterator do b += a
    b.result

  def toArray(chunk: Chunk[Byte]): Array[Byte] =
    chunk.toArray

  override def unsafeWrappedArray(chunk: Chunk[Byte]): Option[Array[Byte]] =
    chunk match
      case Chunk.ArraySlice(array, 0, n) if array.length == n =>
        Some(array)
      case _ =>
        None

  override def copyToArray(chunk: Chunk[Byte], array: Array[Byte], start: Int, len: Int): Int =
    chunk.copyToArray(array, start, len)

  override def toByteBuffer(chunk: Chunk[Byte]): ByteBuffer =
    chunk.toByteBuffer.asReadOnlyBuffer

  def eqv(a: Chunk[Byte], b: Chunk[Byte]): Boolean =
    a == b

  override def toChunk(chunk: Chunk[Byte]): Chunk[Byte] =
    chunk
