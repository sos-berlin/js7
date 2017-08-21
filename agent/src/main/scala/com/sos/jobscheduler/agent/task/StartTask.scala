package com.sos.jobscheduler.agent.task

import com.sos.jobscheduler.data.jobnet.JobPath
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
sealed trait StartTask{
  def jobPath: JobPath
}



final case class StartApiTask(
  jobPath: JobPath,
  javaOptions: String,
  javaClasspath: String)
extends StartTask

object StartApiTask {
  val SerialTypeName = "StartApiTask"
  implicit val MyJsonFormat = jsonFormat3(apply)
}



final case class StartNonApiTask(jobPath: JobPath)
extends StartTask

object StartNonApiTask {
  val SerialTypeName = "StartNonApiTask"
  implicit val MyJsonFormat = jsonFormat1(apply)
}
