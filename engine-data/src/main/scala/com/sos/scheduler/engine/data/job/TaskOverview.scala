package com.sos.scheduler.engine.data.job

import com.sos.scheduler.engine.data.agent.AgentAddress
import com.sos.scheduler.engine.data.processclass.ProcessClassPath
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class TaskOverview(
  id: TaskId,
  jobPath: JobPath,
  state: TaskState,
  processClass: ProcessClassPath,
  agent: Option[AgentAddress] = None)

object TaskOverview {
  implicit val ordering: Ordering[TaskOverview] = Ordering by { _.id }
  implicit private val TaskStateJsonFormat = TaskState.MyJsonFormat
  implicit val MyJsonFormat = jsonFormat5(apply)
}
