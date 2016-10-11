package com.sos.scheduler.engine.data.processclass

import com.sos.scheduler.engine.data.agent.AgentAddress
import scala.collection.immutable
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class ProcessClassDetailed(
  overview: ProcessClassOverview,
  agents: immutable.Seq[AgentAddress])
extends ProcessClassView {

  def path = overview.path
}

object ProcessClassDetailed extends ProcessClassView.Companion[ProcessClassDetailed] {
  implicit val ordering: Ordering[ProcessClassDetailed] = Ordering by { _.path }
  implicit val jsonFormat = jsonFormat2(apply)
}
