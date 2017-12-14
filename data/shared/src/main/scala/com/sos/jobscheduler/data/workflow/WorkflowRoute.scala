package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.base.circeutils.CirceCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCirceCodec
import com.sos.jobscheduler.base.generic.IsString
import com.sos.jobscheduler.base.utils.Collections.implicits.{RichPairTraversable, RichTraversable}
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.workflow.Workflow.{JobNode, Node}
import com.sos.jobscheduler.data.workflow.transition.Transition
import com.sos.jobscheduler.data.workflow.transitions.TransitionRegister
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final case class WorkflowRoute(start: NodeId, end: NodeId, nodes: Seq[Node], transitions: Seq[Transition]) {

  val idToNode = nodes toKeyedMap { _.id }
  val allTransitions: Seq[Transition] = (transitions ++ transitions.flatMap(_.idToRoute.values flatMap (_.allTransitions)))

  require(idToNode.size == nodes.size, s"WorkflowRoute contains duplicate NodeIds")
  (for (t ← allTransitions; to ← t.toNodeIds) yield to → t).uniqueToMap  // throws if not unique

  def requireCompleteness: this.type = {
    require(idToNode isDefinedAt start)
    this
  }

  def reduceForAgent(agentPath: AgentPath): WorkflowRoute = {
    val agentNodes = nodes.collect { case o: JobNode if o.agentPath == agentPath ⇒ o }
    copy(
      transitions = transitions filter (_.endpoints forall (o ⇒ agentNodes.exists(_.id == o))),
      nodes = agentNodes)
  }
}

object WorkflowRoute {
  final case class Id(string: String) extends IsString
  object Id extends IsString.Companion[Id]

  implicit val JsonCodec: CirceCodec[WorkflowRoute] = {
    implicit val TransitionRegisterCodec = TransitionRegister.JsonCodec
    deriveCirceCodec[WorkflowRoute]
  }
}
