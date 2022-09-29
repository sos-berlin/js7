package js7.base.system

import com.sun.management.OperatingSystemMXBean
import java.io.{InputStream, OutputStream}
import scala.annotation.{nowarn, tailrec}

object Java8Polyfill
{
  private val byteBufferSize = 8192

  val javaVersion: Int = {
    // Java 9: = Runtime.version.major
    // Java 10: = Runtime.version.feature
    sys.props.get("java.version")
      .flatMap { v =>
        val parts = v.split("\\.")
        val s = parts(0) match {
          case "1" => parts(1)
          case part0 => part0
        }
        s.toIntOption
      }
      .getOrElse(0)
  }

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

  implicit final class ThreadPolyfill(private val thread: Thread) extends AnyVal {
    @nowarn("msg=method getId in class Thread is deprecated")
    def threadId: Long =
      thread.getId
  }

  implicit final class OperatingSystemMXBeanPolyfill(private val mx: OperatingSystemMXBean)
  extends AnyVal {
    @nowarn("msg=method getId in class Thread is deprecated")
    def getCpuLoad: Double =
      mx.getSystemCpuLoad
  }
}
