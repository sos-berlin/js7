package com.sos.scheduler.engine.data.processclass

import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.scheduler.engine.data.agent.AgentAddress
import com.sos.scheduler.engine.data.job.{JobPath, TaskId, TaskKey}
import java.time.Instant
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class ProcessDetailed(
  jobPath: JobPath,
  taskId: TaskId,
  startedAt: Instant,
  pid: Option[Int],
  agent: Option[AgentAddress])
{
  def taskKey = TaskKey(jobPath, taskId)
}

object ProcessDetailed {
  implicit val jsonFormat = jsonFormat5(apply)
}
