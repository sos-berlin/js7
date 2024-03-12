import BuildUtils.{isMac, isWindows}
import scala.sys.process.*
import scala.util.Try

object BuildCPU {

  lazy val cpu: String =
    if (isMac)
      Try("/usr/sbin/sysctl -n machdep.cpu.brand_string".!!.trim).toOption getOrElse ""
    else if (isWindows)
      ""
    else
      ""
      //Try("""lscpu | grep '^Model name:'|sed -r 's/^Model name:\s+(.+)/»\1«/'""".!!.trim)
      //  .toOption getOrElse ""

  lazy val testParallelization: Int =
    cpu match {
      case "Apple M1 Max" => 16
      case _ => sys.runtime.availableProcessors / 2
    }
}
