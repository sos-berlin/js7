package com.sos.scheduler.engine.common.process

import com.sos.scheduler.engine.common.process.Processes.Pid
import com.sos.scheduler.engine.common.system.OperatingSystem._
import javax.lang.model.SourceVersion

/**
 * Tries to retrieve the PID of a process.
 * With Java 9, these methods become obsolete and can be replaced by Process.getPid.
 *
 * @see https://github.com/flapdoodle-oss/de.flapdoodle.embed.process/blob/master/src/main/java/de/flapdoodle/embed/process/runtime/Processes.java
 * @author Joacim Zschimmer
 */
private[process] object ProcessesJava8pid {
  private[process] val processToPid = SourceVersion.latest.toString match {
    case "RELEASE_9" ⇒ Jdk9ProcessToPid
    case _ ⇒ if (isWindows) WindowsBeforeJava9ProcessToPid else UnixBeforeJava9ProcessToPid
  }

  private object Jdk9ProcessToPid extends (Process ⇒ Option[Pid]) {
    private lazy val getPid = classOf[Process].getMethod("getPid")   // Needs Java 9

    def apply(process: Process) =
      try Some(Pid(getPid.invoke(process).asInstanceOf[java.lang.Long]))
      catch {
        case t: Throwable ⇒ None
      }
  }

  private object UnixBeforeJava9ProcessToPid extends (Process ⇒ Option[Pid]) {
    def apply(process: Process) = try {
      val pidField = process.getClass.getDeclaredField("pid")
      pidField.setAccessible(true)
      Some(Pid(pidField.get(process).asInstanceOf[java.lang.Integer].longValue))
    }
    catch {
      case t: Throwable ⇒ None
    }
  }

  private object WindowsBeforeJava9ProcessToPid extends (Process ⇒ Option[Pid]) {
    def apply(process: Process) = try {
      None // May be implemented with JNA https://github.com/java-native-access/jna !!!
      //val handleField = process.getClass.getDeclaredField("handle")
      //handleField.setAccessible(true)
      //val handle = new WinNT.HANDLE
      //handle.setPointer(Pointer.createConstant(handleField.getLong(process)))
      //Kernel32.INSTANCE.GetProcessId(handle)
    }
    catch {
      case t: Throwable ⇒ None
    }
  }
}
