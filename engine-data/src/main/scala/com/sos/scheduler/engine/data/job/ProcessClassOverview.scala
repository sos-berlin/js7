package com.sos.scheduler.engine.data.job

import com.sos.scheduler.engine.data.filebased.{FileBasedOverview, FileBasedState}
import com.sos.scheduler.engine.data.processclass.ProcessClassPath
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class ProcessClassOverview(
  path: ProcessClassPath,
  fileBasedState: FileBasedState,
  processLimit: Int,
  usedProcessCount: Int)
extends FileBasedOverview {

  def processLimitReached = usedProcessCount >= processLimit
}

object ProcessClassOverview {
  implicit val ordering: Ordering[ProcessClassOverview] = Ordering by { _.path }
  private implicit val FileBasedStateJsonFormat = FileBasedState.MyJsonFormat
  implicit val MyJsonFormat = jsonFormat4(apply)
}
