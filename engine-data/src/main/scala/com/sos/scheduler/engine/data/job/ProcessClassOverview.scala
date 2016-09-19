package com.sos.scheduler.engine.data.job

import com.sos.scheduler.engine.data.filebased.FileBasedState
import com.sos.scheduler.engine.data.processclass.{ProcessClassObstacle, ProcessClassPath}
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class ProcessClassOverview(
  path: ProcessClassPath,
  fileBasedState: FileBasedState,
  processLimit: Int,
  usedProcessCount: Int,
  obstacles: Set[ProcessClassObstacle] = Set()) {

  def processLimitReached = usedProcessCount >= processLimit
}

object ProcessClassOverview {
  implicit val ordering: Ordering[ProcessClassOverview] = Ordering by { _.path }
  private implicit val FileBasedStateJsonFormat = FileBasedState.MyJsonFormat
  implicit val MyJsonFormat = jsonFormat5(apply)
}
