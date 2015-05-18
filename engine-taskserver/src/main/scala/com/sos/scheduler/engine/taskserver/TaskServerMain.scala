package com.sos.scheduler.engine.taskserver

import com.google.common.io.ByteStreams
import com.sos.scheduler.engine.taskserver.task.TaskStartArguments
import spray.json._

/**
 * @author Joacim Zschimmer
 */
object TaskServerMain {

  def main(args: Array[String]): Unit = {
    try {
      val startArguments = new JsonParser(ByteStreams.toByteArray(System.in)).parseJsValue().asJsObject.convertTo[TaskStartArguments]
      SimpleTaskServer.run(startArguments)
    } catch {
      case t: Throwable ⇒
        System.err.println(t.toString)
        t.printStackTrace(System.err)
        System.exit(1)
    }
  }
}
