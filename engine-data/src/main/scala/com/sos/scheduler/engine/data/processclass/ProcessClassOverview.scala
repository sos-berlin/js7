package com.sos.scheduler.engine.data.processclass

import com.sos.scheduler.engine.data.filebased.FileBasedState
import spray.json.DefaultJsonProtocol._
import spray.json.RootJsonFormat

/**
  * @author Joacim Zschimmer
  */
final case class ProcessClassOverview(
  path: ProcessClassPath,
  fileBasedState: FileBasedState,
  processLimit: Int,
  usedProcessCount: Int,
  obstacles: Set[ProcessClassObstacle] = Set())
extends ProcessClassView {

  def processLimitReached = usedProcessCount >= processLimit
}

object ProcessClassOverview extends ProcessClassView.Companion[ProcessClassOverview] {
  implicit val ordering: Ordering[ProcessClassOverview] = Ordering by { _.path }
  implicit val jsonFormat: RootJsonFormat[ProcessClassOverview] = {
    implicit val x = FileBasedState.MyJsonFormat
    jsonFormat5(apply)
  }
}
