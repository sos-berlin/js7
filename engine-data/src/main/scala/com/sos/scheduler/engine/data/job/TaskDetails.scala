package com.sos.scheduler.engine.data.job

import com.sos.scheduler.engine.base.sprayjson.SprayJson.implicits._
import java.nio.file.Path
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class TaskDetails(
  overview: TaskOverview,
  variables: Map[String, String],
  stdoutFile: Path)

object TaskDetails {
  implicit val MyJsonFormat = jsonFormat3(apply)
}
