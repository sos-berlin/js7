package js7.base.utils

import java.io.{InputStream, OutputStream, Reader, Writer}
import scala.annotation.tailrec

object IOUtils
{
  // Java 9: replace by in.transferTo(out)
  def copyStream(in: InputStream, out: OutputStream): Unit = {
    val buffer = new Array[Byte](8192)

    @tailrec def loop(): Unit = {
      val len = in.read(buffer)
      if  (len > 0) {
        out.write(buffer, 0, len)
        loop()
      }
    }
    loop()
  }

  private def copyReaderToWriter(reader: Reader, charBufferSize: Int, writer: Writer): Unit = {
    val array = new Array[Char](charBufferSize)

    @tailrec def loop(): Unit =
      reader.read(array) match {
        case -1 =>
        case len =>
          writer.write(array, 0, len)
          loop()
      }

    try loop()
    finally writer.close()  // Send "end of file"
  }
}
