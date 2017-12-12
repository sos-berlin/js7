package com.sos.jobscheduler.data.workflow.transition

import com.sos.jobscheduler.base.circeutils.CirceCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCirceCodec
import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversable
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.workflow.NodeId
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
  transitionType: TransitionType) {

  if (outlets.size < transitionType.outletsMinimum) throw new IllegalArgumentException(s"$transitionType requires ${transitionType.outletsMinimum} input nodes")
  if (transitionType.outletsMaximum exists (outlets.size > _)) throw new IllegalArgumentException(s"$transitionType supports not more than ${transitionType.outletsMinimum} output nodes")

  val fromNodeIds: Set[NodeId] = fromProcessedNodeIds.toSet ++ forkNodeId
  val toNodeIds: IndexedSeq[NodeId] = outlets.map(_.nodeId)
  val nodeIds: Set[NodeId] = (fromProcessedNodeIds ++ toNodeIds).uniqueToSet

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

  def apply(from: NodeId, to: NodeId): Transition =
    apply(from, to, ForwardTransition)

  def apply(from: NodeId, to: NodeId, transition: SingleInputTransition): Transition =
    apply(Vector(from), Vector(to), transition)

  def apply(from: Iterable[NodeId], to: Iterable[NodeId], transitionType: TransitionType): Transition =
    of(None, from.toIndexedSeq, to.toIndexedSeq, transitionType)

  def forkJoin(forkNodeId: NodeId, joinNodeId: NodeId, outlets: IndexedSeq[Outlet], childEndNodes: IndexedSeq[NodeId], forkTransitionType: TransitionType, joinTransitionType: TransitionType): (Transition, Transition) = {
    val f = fork(from = forkNodeId, outlets = outlets, forkTransitionType)
    val j = join(fromForked = forkNodeId, fromProcessed = childEndNodes, to = joinNodeId, joinTransitionType)
    (f, j)
  }

  /** Forked orders get the IDs "{OrderId}/{Outlet.Id}". */
  def fork(from: NodeId, outlets: IndexedSeq[Outlet], transitionType: TransitionType): Transition =
    new Transition(
      forkNodeId = None,
      fromProcessedNodeIds = Vector(from),
      outlets,
      transitionType)

  def join(fromForked: NodeId, fromProcessed: IndexedSeq[NodeId], to: NodeId, transitionType: TransitionType): Transition =
    of(Some(fromForked), fromProcessed, Vector(to), transitionType)

  private def of(fromForkedId: Option[NodeId], fromProcessed: IndexedSeq[NodeId], to: IndexedSeq[NodeId], transitionType: TransitionType): Transition =
    new Transition(
      forkNodeId = fromForkedId,
      fromProcessedNodeIds = fromProcessed,
      outlets = for (node ← to) yield Outlet(node),
      transitionType)

  final case class Id(string: String)

  implicit def jsonCodec(implicit encoder: Encoder[TransitionType], decoder: Decoder[TransitionType]): CirceCodec[Transition] =
    deriveCirceCodec[Transition]
}
