package js7.common.files

import java.nio.ByteBuffer
import java.nio.channels.{FileChannel, ReadableByteChannel}
import java.nio.file.Path
import java.nio.file.StandardOpenOption.READ
import js7.base.data.{ByteArray, ByteSequence}
import js7.base.log.Logger
import js7.common.files.ChunkFileReader.*

/** Reads ByteSequences from a file. **/
final class ChunkFileReader[ByteSeq](file: Path, fromEnd: Boolean = false)
  (using ByteSeq: ByteSequence[ByteSeq])
extends AutoCloseable:

  private val channel = FileChannel.open(file, READ)
  private val buffer = ByteBuffer.allocate(ChunkSize)

  if fromEnd then
    channel.position(channel.size)

  def close() =
    logger.trace(s"close $file")
    channel.close()

  def read(): ByteSeq =
    channel.read(buffer) match
      case -1 =>
        ByteSeq.empty
      case _ =>
        buffer.flip()
        val chunk = ByteSeq.fromByteBuffer(buffer)
        buffer.clear()
        chunk


object ChunkFileReader:
  private[files] val ChunkSize = 8192
  private val logger = Logger[this.type]
