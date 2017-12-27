package com.sos.jobscheduler.data.workflow.transition

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCirceCodec
import com.sos.jobscheduler.base.circeutils.{CirceCodec, CirceUtils}
import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversable
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.workflow.{NodeId, WorkflowGraph}
import io.circe.{Decoder, Encoder}
import scala.collection.immutable.{IndexedSeq, ListMap, Seq}

/**
  * @author Joacim Zschimmer
  */
final case class Transition private(
  forkNodeId: Option[NodeId] = None,
  fromProcessedNodeIds: IndexedSeq[NodeId],
  toNodeIds: IndexedSeq[NodeId],
  idToGraph: ListMap[WorkflowGraph.Id, WorkflowGraph],
  transitionType: TransitionType) {

  require(toNodeIds.size >= transitionType.graphsMinimum, s"$transitionType requires ${transitionType.graphsMinimum} input nodes")
  require(transitionType.graphsMaximum forall (idToGraph.size <= _), s"$transitionType supports not more than ${transitionType.graphsMinimum} output nodes")

  val fromNodeIds: IndexedSeq[NodeId] = forkNodeId ++: fromProcessedNodeIds
  val endpoints: Set[NodeId] = (fromProcessedNodeIds ++ toNodeIds).uniqueToSet
  val nodes: Seq[WorkflowGraph.Node] = idToGraph.values.flatMap(_.nodes).toVector

  override def toString = {
    val sb = new StringBuilder(50)
    sb ++= fromProcessedNodeIds.mkString(",")
    sb ++= "->"
    transitionType match {
      case ForwardTransition ⇒
      case o ⇒ sb ++= o.toString; sb ++= "->"
    }
    sb ++= toNodeIds.mkString(",")
    sb.toString
  }
}

object Transition {
  type NodeToTransitionableOrder = PartialFunction[NodeId, Order[Order.Transitionable]]

  def apply(from: NodeId, to: NodeId): Transition =
    apply(from, to, ForwardTransition)

  def apply(from: NodeId, to: NodeId, transition: SingleInputTransition): Transition =
    apply(Vector(from), Vector(to), transition)

  def apply(from: Iterable[NodeId], to: Iterable[NodeId], transitionType: TransitionType): Transition =
    of(None, from.toIndexedSeq, to.toIndexedSeq, transitionType)

  def forkJoin(
    forkNodeId: NodeId,
    joinNodeId: NodeId,
    idToGraph: ListMap[WorkflowGraph.Id, WorkflowGraph],
    forkTransitionType: TransitionType,
    joinTransitionType: TransitionType)
  : (Transition, Transition) = {
    val f = fork(forkNodeId, joinNodeId = joinNodeId, idToGraph, forkTransitionType)
    val fromProcessed = for ((id, route) ← idToGraph) yield route.end getOrElse sys.error(s"Forked graph '$id' has no end")
    val j = join(forkNodeId, fromProcessed = fromProcessed.toVector, to = joinNodeId, joinTransitionType)
    (f, j)
  }

  /** Forked orders get the IDs "{OrderId}/{Outlet.Id}". */
  private def fork(from: NodeId, joinNodeId: NodeId, idToGraph: ListMap[WorkflowGraph.Id, WorkflowGraph], transitionType: TransitionType): Transition =
    new Transition(
      forkNodeId = None,
      fromProcessedNodeIds = Vector(from),
      toNodeIds = idToGraph.values.map(_.start).toVector,
      idToGraph,
      transitionType)

  private def join(forkNodeId: NodeId, fromProcessed: IndexedSeq[NodeId], to: NodeId, transitionType: TransitionType): Transition =
    of(Some(forkNodeId), fromProcessed = fromProcessed, Vector(to), transitionType)

  private def of(fromForkedId: Option[NodeId] = None, fromProcessed: IndexedSeq[NodeId],
    to: IndexedSeq[NodeId], transitionType: TransitionType)
  : Transition =
    new Transition(
      forkNodeId = fromForkedId,
      fromProcessedNodeIds = fromProcessed,
      to,
      idToGraph = ListMap.empty,
      transitionType)

  implicit def jsonCodec(implicit encoder: Encoder[TransitionType], decoder: Decoder[TransitionType]): CirceCodec[Transition] = {
    implicit val idToGraphListMapCodec = CirceUtils.listMapCodec[WorkflowGraph.Id, WorkflowGraph](keyName = "id", valueName = "graph")
    deriveCirceCodec[Transition]
  }
}

