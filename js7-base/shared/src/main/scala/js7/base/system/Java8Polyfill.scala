package js7.base.system

import java.io.{InputStream, OutputStream}
import scala.annotation.tailrec

object Java8Polyfill
{
  private val byteBufferSize = 8192

  /** Required when IDE compiles with JDK >8, but code must still be compilable with JDK 8. */
  def java8Polyfill() = {}

  implicit final class InputStreamPolyfill(private val in: InputStream) extends AnyVal
  {
    def transferTo(out: OutputStream): Long = {
      var count = 0L
      val buffer = new Array[Byte](byteBufferSize)

      @tailrec def loop(): Unit = {
        val len = in.read(buffer)
        if  (len > 0) {
          out.write(buffer, 0, len)
          count += len
          loop()
        }
      }
      loop()
      count
    }
  }
}
