package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.base.circeutils.CirceCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCirceCodec
import com.sos.jobscheduler.base.generic.IsString
import com.sos.jobscheduler.base.utils.Collections.RichMap
import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.workflow.Workflow.{JobNode, Node}
import com.sos.jobscheduler.data.workflow.WorkflowRoute._
import com.sos.jobscheduler.data.workflow.transition.{ForwardTransition, Transition}
import com.sos.jobscheduler.data.workflow.transitions.{ForkTransition, JoinTransition, SuccessErrorTransition, TransitionRegister}
import scala.collection.immutable.{Iterable, Seq}

/**
  * @author Joacim Zschimmer
  */
final case class WorkflowRoute(start: NodeId, nodes: Seq[Node], transitions: Seq[Transition]) {

  val idToNode = nodes toKeyedMap { _.id } withNoSuchKey (nodeId ⇒ new NoSuchElementException(s"Unknown NodeId '$nodeId'"))
  val allTransitions: Seq[Transition] = (transitions ++ transitions.flatMap(_.idToRoute.values flatMap (_.allTransitions)))
  /** Linear path of nodes through the WorkflowRoute without forks or branches.
    */
  lazy val linearPath: Option[Seq[NodeId]] = transitions.linearPath(start)

  require(idToNode.size == nodes.size, s"WorkflowRoute contains Nodes with duplicate NodeIds")
  //Not for Goto: (for (t ← allTransitions; to ← t.toNodeIds) yield to → t)
  //  .uniqueToMap { nodeIds ⇒ new DuplicateKeyException(s"Workflow transitions with duplicate NodeIds: ${nodeIds.mkString(", ")}") }

  def requireCompleteness: this.type = {
    require(idToNode isDefinedAt start)
    this
  }

  def reduceForAgent(agentPath: AgentPath): WorkflowRoute = {
    val agentNodes = nodes.collect { case o: JobNode if o.agentPath == agentPath ⇒ o }
    copy(nodes = agentNodes, transitions = transitions filter (_.endpoints forall (o ⇒ agentNodes.exists(_.id == o))))
  }

  def end: Option[NodeId] = linearPath map (_.last)
}

object WorkflowRoute {
  final case class Id(string: String) extends IsString
  object Id extends IsString.Companion[Id]

  implicit val JsonCodec: CirceCodec[WorkflowRoute] = {
    implicit val TransitionRegisterCodec = TransitionRegister.JsonCodec
    deriveCirceCodec[WorkflowRoute]
  }

  private[workflow] implicit class Transitions(val transitions: Iterable[Transition]) extends AnyVal {
    /** Returns None in case of loop */
    def linearPath(start: NodeId): Option[Seq[NodeId]] = {
      val nodeToTransition: Map[NodeId, Transition] =
        transitions.flatMap { t ⇒
          t.transitionType match {
            case ForwardTransition | SuccessErrorTransition | ForkTransition ⇒  // TODO Generalize
              Some(t.fromNodeIds map (_ → t))
            case _ ⇒ None
          }
        }.flatten.uniqueToMap

      def followers(nodeId: NodeId, remaining: Int): List[NodeId] = {
        if (remaining < 0)
          Nil
        else
          nodeId :: (for  {
            transition ← nodeToTransition.get(nodeId).toList
            follower ← followerOfTransition(transition).toList
            x ← followers(follower, remaining - 1)
          } yield x)
      }
      val result = followers(start, transitions.size + 1)
      result.lengthCompare(transitions.size + 1) <= 0 option result
    }

    private def followerOfTransition(transition: Transition): Option[NodeId] =
      transition.transitionType match {
        case ForwardTransition | SuccessErrorTransition | JoinTransition ⇒  // TODO Generalize
          transition.toNodeIds.headOption

        case ForkTransition ⇒
          transitions.collectFirst {
            case t if t.forkNodeId contains transition.fromNodeIds.head ⇒ t.toNodeIds.headOption
          } .flatten
      }
  }
}
