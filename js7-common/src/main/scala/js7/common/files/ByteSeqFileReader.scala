package js7.common.files

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.Path
import java.nio.file.StandardOpenOption.READ
import js7.base.data.ByteSequence
import js7.base.log.Logger
import js7.common.files.ByteSeqFileReader.*

/** Reads ByteSequences from a file. **/
final class ByteSeqFileReader[ByteSeq](file: Path, fromEnd: Boolean = false)
  (using ByteSeq: ByteSequence[ByteSeq])
extends AutoCloseable:

  private val channel = FileChannel.open(file, READ)
  private val buffer = ByteBuffer.allocate(ChunkSize)
  logger.debug(s"↘ opened $file ↘")

  if fromEnd then
    channel.position(channel.size)

  def close() =
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
