package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.taskserver.TaskServer
import com.sos.scheduler.engine.taskserver.task.SeparateProcessTaskServer._
import com.sos.scheduler.engine.taskserver.task.process.RichProcess
import java.io.File

/**
 * @author Joacim Zschimmer
 */
final class SeparateProcessTaskServer(arguments: TaskStartArguments, javaOptions: Seq[String], javaClasspath: String) extends TaskServer {
  private var process: RichProcess = null

  def start() = {
    process = JavaProcess.startJava(
      options = javaOptions,
      classpath = Some(javaClasspath + File.pathSeparator + JavaProcess.OwnClasspath),
      mainClass = com.sos.scheduler.engine.taskserver.Main.getClass.getName stripSuffix "$",  // Strip Scala object class suffix
      arguments = List(s"-controller=${arguments.controllerAddress}"))
  }

  def close() = {
    process.waitForTermination( o ⇒ logger.info(o))
    process.close()
  }

  def kill() = {
    if (process != null) {
      process.kill()
    }
  }
}

object SeparateProcessTaskServer {
  private val logger = Logger(getClass)
}
