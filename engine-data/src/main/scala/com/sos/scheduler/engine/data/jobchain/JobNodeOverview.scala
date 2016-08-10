package com.sos.scheduler.engine.data.jobchain

import com.sos.scheduler.engine.data.job.JobPath
import spray.json._

/**
 * @author Joacim Zschimmer
 */
trait JobNodeOverview extends NodeOverview {
  def nextNodeId: NodeId
  def errorNodeId: NodeId
  def jobPath: JobPath
  def action: JobChainNodeAction
  def orderCount: Int
}

object JobNodeOverview {

  implicit object JobNodeOverviewJsonFormat extends JsonFormat[JobNodeOverview] {
    def write(o: JobNodeOverview) = NodeOverview.MyJsonFormat.write(o)

    def read(json: JsValue) = NodeOverview.MyJsonFormat.read(json) match {
      case o: JobNodeOverview ⇒ o
      case o ⇒ sys.error(s"JobNode expected instead of: $o")
    }
  }
}
