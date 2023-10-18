package js7.common.files

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.Path
import java.nio.file.StandardOpenOption.READ
import js7.base.data.ByteArray
import js7.base.log.Logger
import js7.common.files.ByteArrayReader.*

final class ByteArrayReader(file: Path, fromEnd: Boolean = false)
extends AutoCloseable:

  val channel = FileChannel.open(file, READ)
  val buffer = ByteBuffer.allocate(ChunkSize)

  if fromEnd then
    channel.position(channel.size)

  def close() =
    logger.trace(s"close $file")
    channel.close()

  def read(): ByteArray =
    channel.read(buffer) match
      case -1 => ByteArray.empty
      case _ =>
        buffer.flip()
        val result = ByteArray.fromArray(buffer.array, buffer.position(), buffer.remaining())
        buffer.clear()
        result


object ByteArrayReader:
  private[files] val ChunkSize = 8192
  private val logger = Logger[this.type]
