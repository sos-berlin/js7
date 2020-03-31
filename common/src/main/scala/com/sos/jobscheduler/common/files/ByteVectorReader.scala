package com.sos.jobscheduler.common.files

import com.sos.jobscheduler.common.files.ByteVectorReader._
import com.sos.jobscheduler.common.scalautil.Logger
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.Path
import java.nio.file.StandardOpenOption.READ
import scodec.bits.ByteVector

final class ByteVectorReader(file: Path, fromEnd: Boolean = false)
extends AutoCloseable
{
  val channel = FileChannel.open(file, READ)
  val buffer = ByteBuffer.allocate(ChunkSize)

  if (fromEnd) {
    channel.position(channel.size)
  }

  def close() = {
    logger.trace(s"close $file")
    channel.close()
  }

  def read(): ByteVector =
    channel.read(buffer) match {
      case -1 => ByteVector.empty
      case _ =>
        buffer.flip()
        val result = ByteVector(buffer.array, buffer.position(), buffer.remaining())
        buffer.clear()
        result
    }
}

object ByteVectorReader
{
  private[files] val ChunkSize = 8192
  private val logger = Logger(getClass)
}
