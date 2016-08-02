package com.sos.scheduler.engine.data.job

import com.sos.scheduler.engine.data.filebased.{FileBasedOverview, FileBasedState}
import com.sos.scheduler.engine.data.processclass.ProcessClassPath
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class JobOverview(
  path: JobPath,
  fileBasedState: FileBasedState,
  defaultProcessClass: Option[ProcessClassPath],
  state: JobState,
  isInPeriod: Boolean,
  taskLimit: Int,
  usedTaskCount: Int,
  obstacles: Set[JobObstacle])
extends FileBasedOverview {

  def taskLimitReached = usedTaskCount >= taskLimit
}

object JobOverview {
  implicit val ordering: Ordering[JobOverview] = Ordering by { _.path }
  private implicit val FileBasedStateJsonFormat = FileBasedState.MyJsonFormat
  private implicit val JobStateJsonFormat = JobState.MyJsonFormat
  implicit val MyJsonFormat = jsonFormat8(apply)
}
