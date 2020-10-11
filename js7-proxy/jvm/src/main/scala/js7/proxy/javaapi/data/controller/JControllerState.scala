package js7.proxy.javaapi.data.controller

import io.vavr.control.{Either => VEither}
import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.base.utils.Collections.implicits.RichTraversable
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.controller.data.ControllerState
import js7.data.agent.AgentName
import js7.data.order.{Order, OrderId}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.proxy.javaapi.data.agent.JAgentRef
import js7.proxy.javaapi.data.cluster.JClusterState
import js7.proxy.javaapi.data.common.JJournaledState
import js7.proxy.javaapi.data.common.VavrConverters._
import js7.proxy.javaapi.data.order.JOrder
import js7.proxy.javaapi.data.order.JOrderPredicates.any
import js7.proxy.javaapi.data.workflow.{JWorkflow, JWorkflowId}
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._
import scala.jdk.StreamConverters._

@javaApi
final case class JControllerState(asScala: ControllerState)
extends JJournaledState[JControllerState, ControllerState]
{
  def eventId: Long =
    asScala.eventId

  def clusterState: JClusterState =
    JClusterState(asScala.clusterState)

  def idToWorkflow(workflowId: JWorkflowId): VEither[Problem, JWorkflow] =
    asScala.repo.idTo[Workflow](workflowId.asScala)
      .map(JWorkflow.apply)
      .toVavr

  def pathToWorkflow(workflowPath: WorkflowPath): VEither[Problem, JWorkflow] =
    asScala.repo.pathTo[Workflow](workflowPath)
      .map(JWorkflow.apply)
      .toVavr

  /** Looks up an AgentName in the current version. */
  def nameToAgentRef(name: AgentName): VEither[Problem, JAgentRef] =
    asScala.nameToAgent.checked(name)
      .map(_.agentRef)
      .map(JAgentRef.apply)
      .toVavr

  def orderIds: java.util.Set[OrderId] =
    asScala.idToOrder.keySet.asJava

  def idToOrder(orderId: OrderId): java.util.Optional[JOrder] =
      asScala.idToOrder.get(orderId)
        .map(JOrder.apply)
        .toJava

  /** Looks up an OrderId and returns a Left(Problem) if the OrderId is unknown. */
  def idToCheckedOrder(orderId: OrderId): VEither[Problem, JOrder] =
    asScala.idToOrder.get(orderId)
      .map(JOrder.apply) match {
        case None => VEither.left(Problem(s"Unknown OrderId in JControllerState: ${orderId.string}"))
        case Some(o) => VEither.right(o)
      }

  @Deprecated
  lazy val eagerIdToOrder: java.util.Map[OrderId, JOrder] =
    asScala.idToOrder
      .view.values.map(JOrder.apply)
      .toKeyedMap(_.id)
      .asJava

  def ordersBy(predicate: Order[Order.State] => Boolean): java.util.stream.Stream[JOrder] =
    asScala.idToOrder
      .valuesIterator
      .filter(predicate)
      .map(JOrder.apply)
      .asJavaSeqStream

  def orderIsInCurrentVersionWorkflow: Order[Order.State] => Boolean =
    _.workflowId.versionId == asScala.repo.versionId

  def orderStateToCount(): java.util.Map[Class[_ <: Order.State], java.lang.Integer] =
    orderStateToCount(any)

  def orderStateToCount(predicate: Order[Order.State] => Boolean): java.util.Map[Class[_ <: Order.State], java.lang.Integer] =
    asScala.idToOrder.values.view
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
