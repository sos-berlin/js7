package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.system.OperatingSystem._
import com.sos.scheduler.engine.taskserver.TaskServer
import com.sos.scheduler.engine.taskserver.task.DedicatedProcessTaskServer._
import java.io.File

/**
 * @author Joacim Zschimmer
 */
final class DedicatedProcessTaskServer(arguments: TaskStartArguments, javaOptions: String, javaClasspath: String) extends TaskServer {
  private var process: Process = null

  def start() = {
    val b = new ProcessBuilder(
      JavaExecutable.getPath,
      "-classpath",
      Classpath,
      com.sos.scheduler.engine.taskserver.Main.getClass.getName,
      s"-controller=${arguments.controllerAddress}")
    process = b.start()
  }

  def close() = {}

  def kill() = {
    if (process != null) {
      process.destroyForcibly()
    }
  }
}

object DedicatedProcessTaskServer {
  private val logger = Logger(getClass)

  private lazy val Classpath = systemProperty("java.class.path")
  private lazy val JavaHome = new File(systemProperty("java.home"))

  private lazy val JavaExecutable: File = {
    // See http://stackoverflow.com/questions/4421658/how-can-i-start-a-second-java-process
    val result = if (isWindows) JavaHome / "bin/java.exe" else JavaHome / "bin/java"
    if (!result.exists || result.isDirectory) throw new RuntimeException(s"Missing Java executable expected at $result")
    result
  }

  private def systemProperty(name: String): String = sys.props(name) match {
    case null ⇒ throw new RuntimeException(s"Missing Java system property '$name'")
    case o ⇒ o
  }
}
