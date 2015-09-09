package com.sos.scheduler.engine.taskserver

import com.google.common.io.ByteStreams
import com.sos.scheduler.engine.common.commandline.CommandLineArguments
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.taskserver.data.TaskStartArguments
import spray.json._

/**
 * @author Joacim Zschimmer
 */
object TaskServerMain {

  private val logger = Logger(getClass)

  def main(args: Array[String]): Unit = {
    CommandLineArguments.parse(args) { _.getString("-agent-task-id=") }  // -agent-task-id=.. is only for the kill script and ignored
    try {
      val startArguments = new JsonParser(ByteStreams.toByteArray(System.in)).parseJsValue().asJsObject.convertTo[TaskStartArguments]
      SimpleTaskServer.runAsMain(startArguments)
      logger.info("Terminating")
    } catch {
      case t: Throwable ⇒
        logger.error(s"$t", t)
        System.err.println(t.toString)
        System.exit(1)
    }
  }
}
