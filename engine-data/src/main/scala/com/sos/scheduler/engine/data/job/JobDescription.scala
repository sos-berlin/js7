package com.sos.scheduler.engine.data.job

import spray.json.DefaultJsonProtocol._
import spray.json.RootJsonFormat

/**
  * @author Joacim Zschimmer
  */
final case class JobDescription(
  path: JobPath,
  description: String)
extends JobView

object JobDescription extends JobView.Companion[JobDescription] {
  implicit val jsonFormat: RootJsonFormat[JobDescription] = jsonFormat2(apply)
}
