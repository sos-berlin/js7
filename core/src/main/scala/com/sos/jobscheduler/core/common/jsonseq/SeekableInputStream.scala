package com.sos.jobscheduler.core.common.jsonseq

import java.io.{InputStream, RandomAccessFile}
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
trait SeekableInputStream extends InputStream
{
  def seek(position: Long): Unit
}

object SeekableInputStream {
  def openFile(file: Path): SeekableInputStream =
    new FileSeekableInputStream(new RandomAccessFile(file.toFile, "r"))

  private class FileSeekableInputStream(file: RandomAccessFile) extends SeekableInputStream
  {
    override def close() = file.close()

    def read() =
      file.read()

    override def read(array: Array[Byte], offset: Int, length: Int) =
      file.read(array, offset, length)

    def seek(position: Long) =
      file.seek(position)
  }
}
