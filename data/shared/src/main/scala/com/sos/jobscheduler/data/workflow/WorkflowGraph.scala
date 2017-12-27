package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.base.circeutils.CirceCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCirceCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.generic.IsString
import com.sos.jobscheduler.base.utils.Collections.RichMap
import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.base.utils.DuplicateKeyException
import com.sos.jobscheduler.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.workflow.WorkflowGraph._
import com.sos.jobscheduler.data.workflow.transition.{ForwardTransition, Transition}
import com.sos.jobscheduler.data.workflow.transitions.{ForkTransition, JoinTransition, SuccessErrorTransition, TransitionRegister}
import io.circe.generic.JsonCodec
import scala.collection.immutable.{Iterable, Seq}

/**
  * @author Joacim Zschimmer
  */
final case class WorkflowGraph(start: NodeId, nodes: Seq[Node], transitions: Seq[Transition],
  originalScript: Option[WorkflowScript])
{
  val idToNode = nodes toKeyedMap { _.id } withNoSuchKey (nodeId ⇒ new NoSuchElementException(s"Unknown NodeId '$nodeId'"))
  val allTransitions: Seq[Transition] = (transitions ++ transitions.flatMap(_.idToGraph.values flatMap (_.allTransitions)))
  lazy val linearPath: Option[Seq[NodeId]] = transitions.linearPath(start)

  /** Linear path of nodes through the WorkflowGraph without forks or branches.
    */
  val forkNodeToJoiningTransition = allTransitions.map(o ⇒ o.forkNodeId → o).collect { case (Some(forkNodeId), t) ⇒ forkNodeId → t }
    .toMap withNoSuchKey (k ⇒ new NoSuchElementException(s"No joining transition for forking node '$k'"))

  val nodeToOutputTransition = (for (t ← allTransitions; nodeId ← t.fromProcessedNodeIds) yield nodeId → t)
    .uniqueToMap(duplicates ⇒ new DuplicateKeyException(s"Nodes with duplicate transitions: ${duplicates.mkString(", ")}"))
    .withNoSuchKey(k ⇒ new NoSuchElementException(s"No output transition for WorkflowGraph.Node '$k'"))

  require(idToNode.size == nodes.size, s"WorkflowGraph contains Nodes with duplicate NodeIds")
  //Not for Goto: (for (t ← allTransitions; to ← t.toNodeIds) yield to → t)
  //  .uniqueToMap { nodeIds ⇒ new DuplicateKeyException(s"Workflow transitions with duplicate NodeIds: ${nodeIds.mkString(", ")}") }

  def requireCompleteness: this.type = {
    require(idToNode isDefinedAt start)
    this
  }

  def reduceForAgent(agentPath: AgentPath): WorkflowGraph = {
    val agentNodes = nodes.collect { case o: JobNode if o.agentPath == agentPath ⇒ o }
    copy(
      nodes = agentNodes,
      transitions = transitions filter (_.endpoints forall (o ⇒ agentNodes.exists(_.id == o))),
      originalScript = None)
  }

  def end: Option[NodeId] = linearPath map (_.last)

  def jobNodeOption(nodeId: NodeId): Option[JobNode] =
    idToNode.get(nodeId) collect { case o: JobNode ⇒ o }

  def jobNode(nodeId: NodeId) = idToNode(nodeId).asInstanceOf[JobNode]

  def jobNodeCount = idToNode.values count { _.isInstanceOf[JobNode] }

  def agentPathOption(nodeId: NodeId): Option[AgentPath] =
    idToNode.get(nodeId) collect { case o: WorkflowGraph.JobNode ⇒ o.agentPath }
}

object WorkflowGraph {
  final case class Id(string: String) extends IsString
  object Id extends IsString.Companion[Id]

  implicit val JsonCodec: CirceCodec[WorkflowGraph] = {
    implicit val TransitionRegisterCodec = TransitionRegister.JsonCodec
    deriveCirceCodec[WorkflowGraph]
  }

  sealed trait Node {
    def id: NodeId
  }

  object Node {
    implicit val JsonCodec: CirceCodec[Node] = TypedJsonCodec[Node](
      Subtype[JobNode],
      Subtype[EndNode])
  }

  @JsonCodec
  final case class EndNode(id: NodeId) extends Node

  @JsonCodec
  final case class JobNode(id: NodeId, job: AgentJobPath) extends Node {
    def agentPath = job.agentPath
    def jobPath = job.jobPath
  }
  object JobNode {
    def apply(id: NodeId, agentPath: AgentPath, jobPath: JobPath) =
      new JobNode(id, AgentJobPath(agentPath, jobPath))
  }

  final case class Named(path: WorkflowPath, graph: WorkflowGraph) {
    def start = NodeKey(path, graph.start)
    def toWorkflowScriptNamed: Option[WorkflowScript.Named] =
      for (script ← graph.originalScript) yield WorkflowScript.Named(path, script)
  }
  object Named {
    implicit val jsonCodec = deriveCirceCodec[Named]
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
