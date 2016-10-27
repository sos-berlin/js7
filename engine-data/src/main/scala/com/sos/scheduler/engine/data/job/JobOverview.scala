package com.sos.scheduler.engine.data.job

import com.sos.scheduler.engine.data.filebased.FileBasedState
import com.sos.scheduler.engine.data.processclass.ProcessClassPath
import spray.json.DefaultJsonProtocol._
import spray.json.RootJsonFormat

/**
  * @author Joacim Zschimmer
  */
final case class JobOverview(
  path: JobPath,
  fileBasedState: FileBasedState,
  defaultProcessClassPath: Option[ProcessClassPath],
  state: JobState,
  isInPeriod: Boolean,
  taskLimit: Int,
  usedTaskCount: Int,
  obstacles: Set[JobObstacle])
extends JobView {

  def taskLimitReached = usedTaskCount >= taskLimit
}

object JobOverview extends JobView.Companion[JobOverview] {
  implicit val ordering: Ordering[JobOverview] = Ordering by { _.path }
  implicit val jsonFormat: RootJsonFormat[JobOverview] = {
    implicit val x = FileBasedState.MyJsonFormat
    implicit val y = JobState.MyJsonFormat
    jsonFormat8(apply)
  }
}
