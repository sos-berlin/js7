package com.sos.scheduler.engine.taskserver

import com.google.common.io.ByteStreams
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.taskserver.task.TaskStartArguments
import spray.json._

/**
 * @author Joacim Zschimmer
 */
object TaskServerMain {

  private val logger = Logger(getClass)

  def main(args: Array[String]): Unit = {
    try {
      val startArguments = new JsonParser(ByteStreams.toByteArray(System.in)).parseJsValue().asJsObject.convertTo[TaskStartArguments]
      SimpleTaskServer.run(startArguments)
      logger.info("Terminating")
    } catch {
      case t: Throwable ⇒
        logger.error(s"$t", t)
        System.err.println(t.toString)
        System.exit(1)
    }
  }
}
