package com.sos.scheduler.engine.data.processclass

import com.sos.scheduler.engine.data.agent.AgentAddress
import scala.collection.immutable
import spray.json.DefaultJsonProtocol._
import spray.json.RootJsonFormat

/**
  * @author Joacim Zschimmer
  */
final case class ProcessClassDetailed(
  overview: ProcessClassOverview,
  /**
    *  "FixedPriority" or "RoundRobin", see class `com.sos.scheduler.engine.kernel.processclass.common.selection.SelectionMethod`.
    */
  selectionMethod: String,
  agents: immutable.Seq[AgentAddress],
  processes: immutable.Seq[ProcessDetailed])
extends ProcessClassView {

  def path = overview.path
}

object ProcessClassDetailed extends ProcessClassView.Companion[ProcessClassDetailed] {
  implicit val ordering: Ordering[ProcessClassDetailed] = Ordering by { _.path }
  implicit val jsonFormat: RootJsonFormat[ProcessClassDetailed] = jsonFormat4(apply)
}
