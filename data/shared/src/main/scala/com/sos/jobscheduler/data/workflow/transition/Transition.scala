package com.sos.jobscheduler.data.workflow.transition

import com.sos.jobscheduler.base.circeutils.CirceCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCirceCodec
import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversable
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.workflow.NodeId
import com.sos.jobscheduler.data.workflow.Workflow.Node
import com.sos.jobscheduler.data.workflow.transition.Transition._
import com.sos.jobscheduler.data.workflow.transition.TransitionType.Outlet
import io.circe.{Decoder, Encoder}
import scala.collection.immutable.IndexedSeq

/**
  * @author Joacim Zschimmer
  */
final case class Transition private(
  forkNodeId: Option[NodeId] = None,
  fromProcessedNodeIds: IndexedSeq[NodeId],
  outlets: IndexedSeq[Outlet],
  nodes: IndexedSeq[Node],
  transitionType: TransitionType) {

  nodes.requireUniqueness(_.id)
  if (outlets.size < transitionType.outletsMinimum) throw new IllegalArgumentException(s"$transitionType requires ${transitionType.outletsMinimum} input nodes")
  if (transitionType.outletsMaximum exists (outlets.size > _)) throw new IllegalArgumentException(s"$transitionType supports not more than ${transitionType.outletsMinimum} output nodes")

  val fromNodeIds: Set[NodeId] = fromProcessedNodeIds.toSet ++ forkNodeId
  val toNodeIds: IndexedSeq[NodeId] = outlets.map(_.nodeId)
  val nodeIds: Set[NodeId] = (nodes map (_.id)).toSet

  val id = Id(fromProcessedNodeIds.head.string)   // Unique, because every node is followed by exactly one transition

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

  def apply(from: Node, to: Node): Transition =
    apply(from, to, ForwardTransition)

  def apply(from: Node, to: Node, transition: SingleInputTransition): Transition =
    apply(Vector(from), Vector(to), transition)

  def apply(from: Iterable[Node], to: Iterable[Node], transitionType: TransitionType): Transition =
    of(None, from.toIndexedSeq, to.toIndexedSeq, transitionType)

  def forkJoin(forkNode: Node, joinNode: Node, outlets: IndexedSeq[(Outlet.Id, Node)], childEndNodes: IndexedSeq[Node], forkTransitionType: TransitionType, joinTransitionType: TransitionType): (Transition, Transition) = {
    val f = fork(from = forkNode, to = outlets, forkTransitionType)
    val j = join(fromForked = forkNode.id, fromProcessed = childEndNodes, to = joinNode, joinTransitionType)
    (f, j)
  }

  /** Forked orders get the IDs "{OrderId}/{Outlet.Id}". */
  def fork(from: Node, to: IndexedSeq[(Outlet.Id, Node)], transitionType: TransitionType): Transition =
    new Transition(
      forkNodeId = None,
      fromProcessedNodeIds = Vector(from.id),
      outlets = for ((outletId, node) ← to) yield Outlet(outletId, node.id),
      nodes = (from +: to.map(_._2)).distinct,
      transitionType)

  def join(fromForked: NodeId, fromProcessed: IndexedSeq[Node], to: Node, transitionType: TransitionType): Transition =
    of(Some(fromForked), fromProcessed, Vector(to), transitionType)

  private def of(fromForkedId: Option[NodeId], fromProcessed: IndexedSeq[Node], to: IndexedSeq[Node], transitionType: TransitionType): Transition =
    new Transition(
      forkNodeId = fromForkedId,
      fromProcessedNodeIds = fromProcessed map (_.id),
      outlets = for (node ← to) yield nodeToOutlet(node.id),
      nodes = (fromProcessed ++ to).distinct,
      transitionType)

  private def nodeToOutlet(nodeId: NodeId) = Outlet(Outlet.Id(nodeId.string), nodeId)

  final case class Id(string: String)

  implicit def jsonCodec(implicit encoder: Encoder[TransitionType], decoder: Decoder[TransitionType]): CirceCodec[Transition] =
    deriveCirceCodec[Transition]
}
