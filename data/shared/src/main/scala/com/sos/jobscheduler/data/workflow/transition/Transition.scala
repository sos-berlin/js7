package com.sos.jobscheduler.data.workflow.transition

import com.sos.jobscheduler.base.circeutils.CirceCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCirceCodec
import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversable
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.workflow.{NodeId, Workflow, WorkflowRoute}
import io.circe.{Decoder, Encoder}
import scala.collection.immutable.{IndexedSeq, ListMap, Seq}

/**
  * @author Joacim Zschimmer
  */
final case class Transition private(
  forkNodeId: Option[NodeId] = None,
  fromProcessedNodeIds: IndexedSeq[NodeId],
  toNodeIds: IndexedSeq[NodeId],
  idToRoute: ListMap[WorkflowRoute.Id, WorkflowRoute],
  transitionType: TransitionType) {

  require(toNodeIds.size >= transitionType.routesMinimum, s"$transitionType requires ${transitionType.routesMinimum} input nodes")
  require(transitionType.routesMaximum forall (idToRoute.size <= _), s"$transitionType supports not more than ${transitionType.routesMinimum} output nodes")

  val fromNodeIds: IndexedSeq[NodeId] = forkNodeId ++: fromProcessedNodeIds
  val endpoints: Set[NodeId] = (fromProcessedNodeIds ++ toNodeIds).uniqueToSet
  val nodes: Seq[Workflow.Node] = idToRoute.values.flatMap(_.nodes).toVector

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
    idToRoute: ListMap[WorkflowRoute.Id, WorkflowRoute],
    forkTransitionType: TransitionType,
    joinTransitionType: TransitionType)
  : (Transition, Transition) = {
    val f = fork(forkNodeId, joinNodeId = joinNodeId, idToRoute, forkTransitionType)
    val fromProcessed = for ((id, route) ← idToRoute) yield route.end getOrElse sys.error(s"Forked route '$id' has no end")
    val j = join(forkNodeId, fromProcessed = fromProcessed.toVector, to = joinNodeId, joinTransitionType)
    (f, j)
  }

  /** Forked orders get the IDs "{OrderId}/{Outlet.Id}". */
  private def fork(from: NodeId, joinNodeId: NodeId, idToRoute: ListMap[WorkflowRoute.Id, WorkflowRoute], transitionType: TransitionType): Transition =
    new Transition(
      forkNodeId = None,
      fromProcessedNodeIds = Vector(from),
      toNodeIds = idToRoute.values.map(_.start).toVector,
      idToRoute,
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
      idToRoute = ListMap.empty,
      transitionType)

  implicit def jsonCodec(implicit encoder: Encoder[TransitionType], decoder: Decoder[TransitionType]): CirceCodec[Transition] =
    deriveCirceCodec[Transition]
}

