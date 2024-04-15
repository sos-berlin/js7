package js7.common.jsonseq

import java.io.{InputStream, RandomAccessFile}
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
trait SeekableInputStream extends InputStream:
  def seek(position: Long): Unit


object SeekableInputStream:
  def openFile(file: Path): SeekableInputStream =
    new FileSeekableInputStream(new RandomAccessFile(file.toFile, "r"), file)

  private class FileSeekableInputStream(randomAccess: RandomAccessFile, file: Path) extends SeekableInputStream:
    override def close(): Unit = 
      randomAccess.close()

    def read() =
      randomAccess.read()

    override def read(array: Array[Byte], offset: Int, length: Int) =
      randomAccess.read(array, offset, length)

    def seek(position: Long): Unit =
      randomAccess.seek(position)

    override def toString = file.toString
