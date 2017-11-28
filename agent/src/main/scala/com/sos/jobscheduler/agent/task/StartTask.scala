package com.sos.jobscheduler.agent.task

import com.sos.jobscheduler.data.workflow.JobPath
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
sealed trait StartTask{
  def jobPath: JobPath
}



@JsonCodec
final case class StartApiTask(
  jobPath: JobPath,
  javaOptions: String,
  javaClasspath: String)
extends StartTask

object StartApiTask {
  val SerialTypeName = "StartApiTask"
}



@JsonCodec
final case class StartNonApiTask(jobPath: JobPath)
extends StartTask

object StartNonApiTask {
  val SerialTypeName = "StartNonApiTask"
}
