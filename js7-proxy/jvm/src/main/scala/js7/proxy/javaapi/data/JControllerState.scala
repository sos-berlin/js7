package js7.proxy.javaapi.data

import io.vavr.control.{Either => VEither}
import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.base.utils.Collections.implicits.RichTraversable
import js7.controller.data.ControllerState
import js7.data.order.{Order, OrderId}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.proxy.javaapi.data.JOrderPredicates.any
import js7.proxy.javaapi.utils.VavrConversions._
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._
import scala.jdk.StreamConverters._

@javaApi
final case class JControllerState(underlying: ControllerState)
extends JJournaledState[JControllerState, ControllerState]
{
  def eventId: Long =
    underlying.eventId

  def clusterState: JClusterState =
    JClusterState(underlying.clusterState)

  def idToWorkflow(workflowId: JWorkflowId): VEither[Problem, JWorkflow] =
    underlying.repo.idTo[Workflow](workflowId.underlying)
      .map(JWorkflow.apply)
      .toVavr

  def pathToWorkflow(workflowPath: WorkflowPath): VEither[Problem, JWorkflow] =
    underlying.repo.pathTo[Workflow](workflowPath)
      .map(JWorkflow.apply)
      .toVavr

  def orderIds: java.util.Set[OrderId] =
    underlying.idToOrder.keySet.asJava

  def idToOrder(orderId: OrderId): java.util.Optional[JOrder] =
      underlying.idToOrder.get(orderId)
        .map(JOrder.apply)
        .toJava

  @Deprecated
  lazy val eagerIdToOrder: java.util.Map[OrderId, JOrder] =
    underlying.idToOrder
      .view.values.map(JOrder.apply)
      .toKeyedMap(_.id)
      .asJava

  def ordersBy(predicate: Order[Order.State] => Boolean): java.util.stream.Stream[JOrder] =
    underlying.idToOrder
      .valuesIterator
      .filter(predicate)
      .map(JOrder.apply)
      .asJavaSeqStream

  def orderStateToCount(): java.util.Map[Class[_ <: Order.State], java.lang.Integer] =
    orderStateToCount(any)

  def orderStateToCount(predicate: Order[Order.State] => Boolean): java.util.Map[Class[_ <: Order.State], java.lang.Integer] =
    underlying.idToOrder.values.view
      .filter(predicate)
      .groupBy(_.state.getClass)
      .view.mapValues(o => java.lang.Integer.valueOf(o.size))
      .toMap.asJava
}

object JControllerState
{
  implicit val companion = new JJournaledState.Companion[JControllerState, ControllerState] {
    def apply(underlying: ControllerState) = new JControllerState(underlying)
  }
}
