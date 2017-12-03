package com.sos.jobscheduler.data.workflow.transition

import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversable
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.jobscheduler.data.order.OrderEvent
import com.sos.jobscheduler.data.workflow.Workflow.Node
import com.sos.jobscheduler.data.workflow.transition.Transition._
import com.sos.jobscheduler.data.workflow.transition.TransitionRegister.JsonCodec
import com.sos.jobscheduler.data.workflow.{NodeId, NodeToLeanOrder}
import io.circe.generic.JsonCodec
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class Transition(fromNodeIds: Seq[NodeId], toNodeIds: Seq[NodeId], nodes: Seq[Node], transitionType: TransitionType) {

  // TODO Wir brauchen nur die NodeId, nicht die ganze Node

  nodes toKeyedMap (_.id)  // throws DuplicateKeyException

  def nodeIds = (nodes map (_.id)).toSet

  val id = Id(fromNodeIds.head.string)   // Unique, because every node is followed by exactly one transition

  def switch(nodeToOrder: NodeToLeanOrder): Option[OrderEvent.OrderTransitioned] =
    transitionedResults(nodeToOrder) /*match {
      case Seq() ⇒ Nil
      case results ⇒
        for ((resultOption, nodeId) ← results zip toNodeIds) yield
          for (result ← resultOption) yield
            LeanOrder(result.id, nodeId, Order.Ready, result.payload)
    }*/

  private def transitionedResults(nodeToOrder: NodeToLeanOrder): Option[OrderEvent.OrderTransitioned] =
    canSwitch(nodeToOrder) option {
      val nodeIdIndex = transitionType.results(fromNodeIds map nodeToOrder)
      if (!toNodeIds.indices.contains(nodeIdIndex)) sys.error(s"Transition $transitionType returns wrong following order index: $nodeIdIndex, expected one of ${toNodeIds.indices}")
      val toNodeId = toNodeIds(transitionType.results(fromNodeIds map nodeToOrder))
      OrderEvent.OrderTransitioned(toNodeId)
    }

  def canSwitch(nodeToFragment: NodeToLeanOrder): Boolean =
    fromNodeIds forall nodeToFragment.isDefinedAt

  override def toString = {
    val sb = new StringBuilder(50)
    sb ++= fromNodeIds.mkString(",")
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
  intelliJuseImport(/*TransitionRegister.*/JsonCodec)

  def apply(from: Node, to: Node): Transition =
    apply(from, to, ForwardTransition)

  def apply(from: Node, to: Node, transition: OneToOneTransition): Transition =
    apply(from :: Nil, to :: Nil, transition)

  def apply(from: Seq[Node], to: Seq[Node], transitionType: TransitionType): Transition =
    new Transition(from map (_.id), to map (_.id), (from ++ to).distinct, transitionType)

  final case class Id(string: String)
}
