package js7.launcher.crashpidfile

import cats.effect.{IO, Resource, ResourceIO}
import java.io.RandomAccessFile
import java.nio.ByteOrder.BIG_ENDIAN
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.charset.StandardCharsets.US_ASCII
import java.nio.file.Path
import java.nio.{ByteBuffer, ByteOrder}
import js7.base.catsutils.CatsEffectExtensions.defer
import js7.base.thread.IOExecutor.env.interruptibleVirtualThread
import js7.base.utils.AsyncLock
import js7.base.utils.ScalaUtils.syntax.*
import js7.launcher.crashpidfile.IndexedRecordSetImpl.*
import scala.collection.mutable

/** A mutable Set[A], stored in an indexed thing, reusing the freed entries. */
private final class IndexedRecordSetImpl[A] private(writeAtIndex: WriteAtIndex[A], label: String)
extends IndexedRecordSet[A]:

  private val lock = AsyncLock()
  private val aToIndex = mutable.Map.empty[A, Integer]
  private var size = 0
  private val freeIndices = mutable.BitSet()

  def register(a: A): ResourceIO[Unit] =
    Resource:
      add(a).as:
        () -> remove(a)

  private def add(a: A): IO[Unit] =
    lock.lock:
      IO.defer:
        val index = freeIndices.headOption getOrElse:
          size += 1
          size - 1
        freeIndices -= index
        aToIndex(a) = index
        writeAtIndex(index, a)

  private def remove(a: A): IO[Unit] =
    lock.lock:
      IO.defer:
        aToIndex.remove(a).fold(IO.unit): index_ =>
          val index = index_.intValue
          freeIndices += index
          writeAtIndex(index, Delete)

  override def toString =
    "IndexedRecordSetImpl" + (label.nonEmpty ?? s":$label")


private object IndexedRecordSetImpl:

  type WriteAtIndex[A] = (Int, A | Delete) => IO[Unit]
  type Delete = Delete.type
  object Delete

  def apply[A](label: String = "")(writeAtIndex: WriteAtIndex[A]): IndexedRecordSetImpl[A] =
    new IndexedRecordSetImpl(writeAtIndex, label)

  /** Creates an IndexedRecordSetImpl based on a fixed length text record file.
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
  : ResourceIO[IndexedRecordSetImpl[A]] =
    Resource.defer:
      val emptyLine = (" " * stringByteSize + '\n').getBytes(US_ASCII)
      file[A](path, stringByteSize + 1, label = label):
        case (buf, Delete) =>
          buf.put(emptyLine)
        case (buf, a: A @unchecked) =>
          val pos0 = buf.position()
          writeBuffer(buf, a)
          val written = buf.position - pos0
          buf.put(emptyLine, written, emptyLine.length - written)

  /** Creates an IndexedRecordSetImpl based on a fixed length record file.
   * @param recordSize The fixed number of bytes of each record.
   * @param writeBuffer Fills a ByteBuffer with exactly recordSize bytes.
   * @param label For logging.
   */
  def file[A](file: Path, recordSize: Int, byteOrder: ByteOrder = BIG_ENDIAN, label: String = "")
    (writeBuffer: (ByteBuffer, A | Delete) => Unit)
  : ResourceIO[IndexedRecordSetImpl[A]] =
    for
      channel <- fileChannelResource(file)
      service <- Resource.defer:
        channel.truncate(0)
        val myLabel = if label.nonEmpty then label else file.toString
        Resource.eval(IO:
          IndexedRecordSetImpl[A](myLabel):
            new WriteAtIndex[A]:
              private val byteBuffer = ByteBuffer.wrap(new Array(recordSize)).order(byteOrder)
              private val lock = AsyncLock()

              def apply(index: Int, a: A | Delete) =
                lock.lock:
                  IO.defer:
                    byteBuffer.clear()
                    writeBuffer(byteBuffer, a)
                    if byteBuffer.position != recordSize then throw AssertionError:
                      s"Unexpected length=${byteBuffer.position}, expected was $recordSize"
                    byteBuffer.flip()
                    interruptibleVirtualThread:
                      channel.position(index * recordSize)
                      channel.write(byteBuffer)
                      ())
    yield
      service

  private def fileChannelResource(path: Path): Resource[IO, FileChannel] =
    Resource:
      IO(RandomAccessFile(path.toFile, "rw").getChannel)
        .map: channel =>
          channel -> IO(channel.close())
