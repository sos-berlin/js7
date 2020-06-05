package js7.common.process

import javax.lang.model.SourceVersion
import js7.base.utils.ScalaUtils.RichThrowable
import js7.common.process.Processes.Pid
import js7.common.scalautil.Logger
import js7.common.system.OperatingSystem._

/**
 * Tries to retrieve the PID of a process.
 * With Java 9, these methods become obsolete and can be replaced by Process.getPid.
 *
 * @see https://github.com/flapdoodle-oss/de.flapdoodle.embed.process/blob/master/src/main/java/de/flapdoodle/embed/process/runtime/Processes.java
 * @author Joacim Zschimmer
 */
object ProcessPidRetriever
{
  private[process] val hasJava9 = SourceVersion.values map { _.toString } contains "RELEASE_9"
  lazy val maybeOwnPid: Option[Pid] =
    if (!hasJava9)
      None
    else
      try {
        val processHandleClass = Class.forName("java.lang.ProcessHandle")
        val currentProcessHandle = processHandleClass.getMethod("current").invoke(null)
        Some(Pid(
          processHandleClass
            .getMethod("pid")
            .invoke(currentProcessHandle)
            .asInstanceOf[Long]))
      } catch {
        case t: Throwable =>
          logger.debug(s"maybeOwnPid => ${t.toStringWithCauses}")
          None
      }
  private val logger = Logger(getClass)

  private[process] val processToPid: Process => Option[Pid] =
    if (hasJava9)
      Java9ProcessToPid
    else if (isWindows)
      WindowsBeforeJava9ProcessToPid
    else
      UnixBeforeJava9ProcessToPid

  private object Java9ProcessToPid extends (Process => Option[Pid]) {
    private val pidMethod = classOf[Process].getMethod("pid")   // Needs Java 9
    private var logged = false

    def apply(process: Process) =
      try Some(Pid(pidMethod.invoke(process).asInstanceOf[java.lang.Long]))
      catch { case t: Throwable =>
        if (!logged) {
          logged = true
          logger.error(s"(Logged only once) Process.pid: ${t.toStringWithCauses}", t)
        }
        None
      }
  }

  private object UnixBeforeJava9ProcessToPid extends (Process => Option[Pid]) {
    def apply(process: Process) =
      try {
        val pidField = process.getClass.getDeclaredField("pid")
        pidField.setAccessible(true)
        Some(Pid(pidField.get(process).asInstanceOf[java.lang.Integer].longValue))
      } catch { case _: Throwable =>
        None
      }
  }

  private object WindowsBeforeJava9ProcessToPid extends (Process => Option[Pid]) {
    def apply(process: Process) =
      try {
        None // May be implemented with JNA https://github.com/java-native-access/jna !!!
        //val handleField = process.getClass.getDeclaredField("handle")
        //handleField.setAccessible(true)
        //val handle = new WinNT.HANDLE
        //handle.setPointer(Pointer.createConstant(handleField.getLong(process)))
        //Kernel32.INSTANCE.GetProcessId(handle)
      } catch { case _: Throwable =>
        None
      }
    }
}
