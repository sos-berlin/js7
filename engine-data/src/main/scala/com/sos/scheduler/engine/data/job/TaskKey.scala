package com.sos.scheduler.engine.data.job

import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class TaskKey(jobPath: JobPath, taskId: TaskId)

object TaskKey {
  implicit val MyJsonFormat = jsonFormat2(apply)
}
