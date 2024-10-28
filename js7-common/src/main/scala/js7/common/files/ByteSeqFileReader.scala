package js7.common.files

import cats.effect.{IO, Resource}
import fs2.{Chunk, Stream}
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.StandardOpenOption.READ
import java.nio.file.{Files, Path}
import js7.base.data.ByteSequence
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.fs2utils.StreamExtensions.takeWhileNotNull
import js7.base.log.Logger
import js7.common.files.ByteSeqFileReader.*
import scala.concurrent.duration.FiniteDuration

/** Reads ByteSequences from a file. **/
final class ByteSeqFileReader[ByteSeq](file: Path, fromEnd: Boolean = false)
  (using ByteSeq: ByteSequence[ByteSeq])
extends AutoCloseable:

  private val channel = FileChannel.open(file, READ)
  private val buffer = ByteBuffer.allocate(ChunkSize)
  logger.debug(s"↘ opened $file ↘")

  if fromEnd then
    channel.position(channel.size)

  def close(): Unit =
    logger.debug(s"↙ close $file ↙")
    channel.close()

  def read(): ByteSeq =
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

  def fileStream(
    file: Path,
    pollDuration: Option[FiniteDuration],
    fromEnd: Boolean = false)
  : Stream[IO, Byte] =
    pollDuration match
      case None =>
        endlessStream(file, fromEnd)
          .takeWhile(_.nonEmpty)
          .unchunks

      case Some(d) =>
        growingFileStream(file, d, fromEnd = fromEnd)

  def fileStream(file: Path): Stream[IO, Byte] =
    endlessStream(file)
      .takeWhile(_.nonEmpty)
      .unchunks

  def growingFileStream(file: Path, pollDuration: FiniteDuration, fromEnd: Boolean = false)
  : Stream[IO, Byte] =
    endlessStream(file, fromEnd = fromEnd)
      .evalMap: chunk =>
        if chunk.nonEmpty then
          IO.pure(chunk)
        else if Files.exists(file) then
          IO.sleep(pollDuration).as(Chunk.empty) // Delay and try again
        else
          IO.pure(null /*end of stream*/)
      .takeWhileNotNull
      .filter(_.nonEmpty)
      .unchunks

  private def endlessStream(file: Path, fromEnd: Boolean = false): Stream[IO, Chunk[Byte]] =
    Stream
      .resource:
        Resource:
          IO.interruptible:
            val reader = new ByteSeqFileReader[Chunk[Byte]](file, fromEnd = fromEnd)
            reader -> IO.blocking:
              reader.close()
      .flatMap: reader =>
        Stream.repeatEval:
          IO.interruptible:
            reader.read()
