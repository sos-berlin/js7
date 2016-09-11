package com.sos.scheduler.engine.data.job

import com.sos.scheduler.engine.data.agent.AgentAddress
import com.sos.scheduler.engine.data.processclass.ProcessClassPath
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class TaskOverview(
  taskId: TaskId,
  jobPath: JobPath,
  state: TaskState,
  processClassPath: Option[ProcessClassPath],
  agent: Option[AgentAddress] = None) {

  def taskKey = TaskKey(jobPath, taskId)
}

object TaskOverview {
  implicit val ordering: Ordering[TaskOverview] = Ordering by { _.taskId }
  implicit private val TaskStateJsonFormat = TaskState.MyJsonFormat
  implicit val MyJsonFormat = jsonFormat5(apply)
}
