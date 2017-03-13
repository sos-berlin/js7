package com.sos.jobscheduler.agent.data.commands

import com.sos.jobscheduler.agent.data.commandresponses.StartTaskResponse
import com.sos.jobscheduler.agent.data.commands.StartTask.Meta
import com.sos.jobscheduler.data.job.TaskId
import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
trait StartTask extends TaskCommand {
  type Response = StartTaskResponse

  def meta: Option[Meta]
}

object StartTask {
  final case class Meta(
    job: String,
    taskId: TaskId)

  object Meta {
    /** For compatibility with a master before v1.10.4 **/
    val Default = Meta(job = "/(OLD-MASTER)", TaskId(-1))
    implicit val MyJsonFormat = jsonFormat2(apply)
    val NoCppJobSchedulerTaskId = TaskId(0)   // Not the old C++ JobScheduler
  }
}
