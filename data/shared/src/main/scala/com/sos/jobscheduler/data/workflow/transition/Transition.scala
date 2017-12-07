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
  fromForkedNodeId: Option[NodeId] = None,
  fromProcessedNodeIds: IndexedSeq[NodeId],
  outlets: IndexedSeq[Outlet],
  nodes: IndexedSeq[Node],
  transitionType: TransitionType) {

  nodes.requireUniqueness(_.id)
  if (outlets.size < transitionType.outletsMinimum) throw new IllegalArgumentException(s"$transitionType requires ${transitionType.outletsMinimum} input nodes")
  if (transitionType.outletsMaximum exists (outlets.size > _)) throw new IllegalArgumentException(s"$transitionType supports not more than ${transitionType.outletsMinimum} output nodes")

  val fromNodeIds: Set[NodeId] = fromProcessedNodeIds.toSet ++ fromForkedNodeId
  val toNodeIds: IndexedSeq[NodeId] = outlets map (_.nodeId)
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

  def apply(from: IndexedSeq[Node], to: IndexedSeq[Node], transitionType: TransitionType): Transition =
    of(None, from, to, transitionType)

  def join(fromForked: Node, fromProcessed: IndexedSeq[Node], to: IndexedSeq[Node], transitionType: TransitionType): Transition =
    of(Some(fromForked), fromProcessed, to, transitionType)

  private def of(fromForked: Option[Node], fromProcessed: IndexedSeq[Node], to: IndexedSeq[Node], transitionType: TransitionType): Transition =
    new Transition(
      fromForkedNodeId = fromForked map (_.id),
      fromProcessedNodeIds = fromProcessed map (_.id),
      outlets = for (node ← to) yield Outlet(Outlet.Id(node.id.string), node.id),
      nodes = (fromProcessed ++ to).distinct, transitionType)

  final case class Id(string: String)

  implicit def jsonCodec(implicit encoder: Encoder[TransitionType], decoder: Decoder[TransitionType]): CirceCodec[Transition] =
    deriveCirceCodec[Transition]
}
