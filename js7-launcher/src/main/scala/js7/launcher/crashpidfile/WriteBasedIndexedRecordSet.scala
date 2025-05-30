package js7.launcher.crashpidfile

import cats.effect.{IO, Resource, ResourceIO}
import cats.syntax.option.*
import java.io.RandomAccessFile
import java.nio.ByteOrder.BIG_ENDIAN
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets.US_ASCII
import java.nio.file.Path
import java.nio.{ByteBuffer, ByteOrder}
import js7.base.catsutils.CatsEffectExtensions.defer
import js7.base.log.Logger
import js7.base.utils.AsyncLock
import js7.base.utils.ScalaUtils.syntax.*
import js7.launcher.crashpidfile.WriteBasedIndexedRecordSet.*
import scala.collection.mutable

/** A mutable Set[A], stored in an indexed writable thing, reusing the freed entries.
  *
  * @param writeAtIndex Writes or deletes an `A` in the writeable thing.
  */
private final class WriteBasedIndexedRecordSet[A](
  label: String,
  writeAtIndex: (Int, A | Delete) => IO[Unit])
extends IndexedRecordSet[A]:

  private val lock = AsyncLock()
  private val aToIndex = mutable.Map.empty[A, Integer]
  private var size = 0
  private val freeIndices = mutable.BitSet()
  private var throwable = none[Throwable]

  def register(a: A): ResourceIO[Unit] =
    Resource:
      add(a).as:
        () -> remove(a)

  private def add(a: A): IO[Unit] =
    manipulate(a, "add"):
      val index = freeIndices.headOption getOrElse:
        size += 1
        size - 1
      freeIndices -= index
      aToIndex(a) = index
      writeAtIndex(index, a)

  def remove(a: A): IO[Unit] =
    manipulate(a, "remove"):
      aToIndex.remove(a).fold(IO.unit): index_ =>
        val index = index_.intValue
        freeIndices += index

        // Delete free entries at end of file
        var newSize = size
        while newSize > 0 && freeIndices(newSize - 1) do
          newSize -= 1
          freeIndices -= newSize
        val truncate = (newSize < size) ? newSize
        size = newSize
        writeAtIndex(index, Delete(truncate))

  private def manipulate(a: A, name: String)(body: => IO[Unit]): IO[Unit] =
    lock.lock:
      IO.defer:
        throwable match
          case Some(t) => IO(logger.warn:
            s"$toString $name($a) ignored due to previous error $t")
          case None =>
            IO.defer(body)
              .onError:
                case t => IO:
                  throwable = t.some


  override def toString =
    "WriteBasedIndexedRecordSet" + (label.nonEmpty ?? s":$label")


object WriteBasedIndexedRecordSet:

  private val logger = Logger[this.type]

  final case class Delete(truncate: Option[Int])

  /** Creates an WriteBasedIndexedRecordSet based on a fixed length text record file.
   * <ul>
   * <li>The service terminates each record with a '\n' character.
   * <li> Deleted record consists of stringByteSize spaces and a '\n' character.
   * </ul>
   * @param stringByteSize The fixed number of bytes of the string representation of A.
   *                       The recordSize is + 1 for the always written terminal '\n' character.
   * @param writeBuffer Fills a ByteBuffer with not more than stringByteSize bytes.
   * @param label For logging.
   */
  def textFile[A](path: Path, stringByteSize: Int, label: String = "")
    (writeBuffer: (ByteBuffer, A) => Unit)
  : ResourceIO[WriteBasedIndexedRecordSet[A]] =
    Resource.defer:
      val emptyLine = (" " * stringByteSize + '\n').getBytes(US_ASCII)
      file[A](path, stringByteSize + 1, label = label):
        case (buf, _: Delete) =>
          buf.put(emptyLine)
        case (buf, a: A @unchecked) =>
          val pos0 = buf.position()
          writeBuffer(buf, a)
          val written = buf.position - pos0
          buf.put(emptyLine, written, emptyLine.length - written)

  /** Creates an WriteBasedIndexedRecordSet based on a fixed length record file.
   * @param recordSize The fixed number of bytes of each record.
   * @param writeBuffer Fills a ByteBuffer with exactly recordSize bytes.
   * @param label For logging.
   */
  def file[A](file: Path, recordSize: Int, byteOrder: ByteOrder = BIG_ENDIAN, label: String = "")
    (writeBuffer: (ByteBuffer, A | Delete) => Unit)
  : ResourceIO[WriteBasedIndexedRecordSet[A]] =
    for
      channel <- fileChannelResource(file)
      service <- Resource.defer:
        channel.truncate(0)
        val myLabel = if label.nonEmpty then label else file.toString
        Resource.eval(IO:
          val byteBuffer = ByteBuffer.wrap(new Array(recordSize)).order(byteOrder)
          new WriteBasedIndexedRecordSet[A](
            label = myLabel,
            writeAtIndex = (index, aOrDelete) =>
              IO.defer:
                byteBuffer.clear()
                writeBuffer(byteBuffer, aOrDelete)
                if byteBuffer.position != recordSize then throw AssertionError:
                  s"Unexpected length=${byteBuffer.position}, expected was $recordSize"
                byteBuffer.flip()
                IO.blocking:
                  channel.position(index * recordSize)
                  channel.write(byteBuffer)
                  aOrDelete match
                    case Delete(Some(truncate)) =>
                      channel.truncate(truncate * recordSize)
                    case _ =>
                  ()))
    yield
      service

  private def fileChannelResource(path: Path): Resource[IO, FileChannel] =
    Resource:
      IO(RandomAccessFile(path.toFile, "rw").getChannel)
        .map: channel =>
          channel -> IO(channel.close())
