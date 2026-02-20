package js7.common.files

import cats.effect.{IO, Resource, ResourceIO}
import fs2.{Chunk, Stream}
import izumi.reflect.Tag
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.StandardOpenOption.READ
import java.nio.file.{Files, NoSuchFileException, Path}
import js7.base.data.ByteSequence
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.StreamExtensions.takeWhileNotNull
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixlike.MonixLikeExtensions.onErrorRestartLoop
import js7.base.time.ScalaTime.*
import js7.common.files.ByteSeqFileReader.*
import scala.collection.immutable.VectorBuilder
import scala.concurrent.duration.FiniteDuration

/** Reads ByteSequences from a file. **/
final class ByteSeqFileReader[ByteSeq](file: Path)
  (using ByteSeq: ByteSequence[ByteSeq])
extends AutoCloseable:

  private val channel = FileChannel.open(file, READ)
  private val buffer = ByteBuffer.allocate(ChunkSize)
  private var _next: ByteSeq | Null = null
  private var nextPosition: Long = 0

  def close(): Unit =
    channel.close()

  def size(): Long =
    channel.size()

  //def position: Long =
  //  _next match
  //    case null => channel.position
  //    case byteSeq: ByteSeq @unchecked => channel.position - byteSeq.length

  def seekToEnd(): Unit =
    setPosition(size())

  def setPosition(position: Long): Unit =
    _next = null
    channel.position(position)

  def peek(): ByteSeq =
    _next match
      case null =>
        val r = readBuffer(buffer)
        if r.nonEmpty then
          _next = r
        r
      case byteSeq: ByteSeq @unchecked =>
        byteSeq

  def read(size: Int): ByteSeq =
    if _next != null then throw IllegalStateException("read(size) called after peek()")
    readBuffer(ByteBuffer.allocate(size))

  def read(): ByteSeq =
    _next match
      case null =>
        readBuffer(buffer)
      case byteSeq: ByteSeq @unchecked =>
        _next = null
        byteSeq

  private def readBuffer(buffer: ByteBuffer): ByteSeq =
    buffer.clear()
    channel.read(buffer) match
      case -1 =>
        ByteSeq.empty
      case _ =>
        buffer.flip()
        ByteSeq.readByteBuffer(buffer)


object ByteSeqFileReader:
  private[files] val ChunkSize = 64*1024
  private val logger = Logger[this.type]

  /** Number of first bytes of a log file with a timestamp which should uniquely identify it.
    *
    * Each log file must start with a timestamp including its timezone, to uniquely identify it.
    *
    * <p>
    *   See log4j2.xml header setting. Something like:
    * <pre>
    *%d{yyyy-MM-dd HH:mm:ss.SSSXX} Begin JS7 %X{js7.serverId} · ...%n
    * </pre>
    */
  val UniqueHeaderSize = 50 // First bytes of the log file, see log4j2.xml header setting.

  def fileStream[ByteSeq: ByteSequence](file: Path, byteChunkSize: Int): Stream[IO, Chunk[ByteSeq]] =
    Stream.resource:
      fileReaderResource(file)
    .flatMap:
      chunkStream(_, byteChunkSize)
    .takeWhile(_.nonEmpty)

  def growingLogFileStream[ByteSeq: {ByteSequence, Tag}](
    file: Path,
    byteChunkSize: Int,
    pollDuration: FiniteDuration,
    fromEnd: Boolean = false)
  : Stream[IO, Chunk[ByteSeq]] =
    logger.debugStream:
      growingLogFileStream2[ByteSeq](file, byteChunkSize, pollDuration, fromEnd)

  private def growingLogFileStream2[ByteSeq: {ByteSequence, Tag}](
    file: Path,
    byteChunkSize: Int,
    pollDuration: FiniteDuration,
    fromEnd: Boolean = false)
  : Stream[IO, Chunk[ByteSeq]] =
    Stream.resource:
      fileReaderResource(file)
    .flatMap: reader =>
      Stream.eval:
        IO.blocking:
          val header = reader.read(UniqueHeaderSize)
          if fromEnd then
            reader.seekToEnd()
          else
            reader.setPosition(0)
          header
      .flatMap: header =>
        if header.length < UniqueHeaderSize then
          Stream.sleep_[IO](pollDuration) // Delay and end stream. Caller will try again.
        else
          chunkStream(reader, byteChunkSize).flatMap: byteSeqs =>
            if byteSeqs.nonEmpty then
              Stream.emit(byteSeqs)
            else
              Stream.sleep_[IO](pollDuration).append:
                // TODO Wait longer before reading header again
                Stream.force:
                  // When the log file chaned, its header file changed, too
                  readHeader[ByteSeq](file).map:
                    case `header` =>
                      Stream.empty // Unchanged, continue
                    case h =>
                      logger.debug(s"Log file header has changed: ${h.utf8String}")
                      Stream.emit(null) // Changed, end the stream
            end if
          .takeWhileNotNull
    .append:
      waitUntilFileExists(file, pollDuration)
    .append:
      growingLogFileStream(file, byteChunkSize, pollDuration)

  private def fileReaderResource[ByteSeq: ByteSequence](file: Path, fromEnd: Boolean = false)
  : ResourceIO[ByteSeqFileReader[ByteSeq]] =
    Resource.fromAutoCloseable:
      IO.blocking:
        val reader = ByteSeqFileReader[ByteSeq](file)
        if fromEnd then
          reader.seekToEnd()
        reader
      .onErrorRestartLoop(()):
        case (e: NoSuchFileException, _, retry) =>
          logger.debug(s"$file: $e")
          retry(()).delayBy(1.s)
        case (t, _, _) => IO.raiseError(t)

  private def waitUntilFileExists(file: Path, pollDuration: FiniteDuration): Stream[IO, Nothing] =
    logger.traceCall("waitUntilFileExists", file):
      Stream.eval:
        IO.blocking:
          Files.exists(file)
      .evalTap: exists =>
        IO.unlessA(exists):
          IO.sleep(pollDuration)
      .collectFirst:
        case true =>
      .drain

  private def readHeader[ByteSeq: ByteSequence](file: Path): IO[ByteSeq] =
    logger.traceIO("readHeader"):
      fileReaderResource[ByteSeq](file).use: reader =>
        IO.blocking:
          reader.read(UniqueHeaderSize)

  private def chunkStream[ByteSeq: ByteSequence](
    reader: ByteSeqFileReader[ByteSeq],
    byteChunkSize: Int)
  : Stream[IO, Chunk[ByteSeq]] =
    Stream.repeatEval:
      IO.interruptible:
        val b = new VectorBuilder[ByteSeq]
        var size = 0
        while size < byteChunkSize
          && (size == 0 || size + reader.peek().length <= byteChunkSize)
          && reader.peek().nonEmpty
        do
          val byteSeq = reader.read()
          b += byteSeq
          size += byteSeq.length
        Chunk.from(b.result())
