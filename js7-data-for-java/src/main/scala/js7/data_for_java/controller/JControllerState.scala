package  js7.data_for_java.controller

import io.vavr.control.{Either => VEither}
import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.problem.Problem
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.data.agent.AgentId
import js7.data.controller.ControllerState
import js7.data.lock.LockId
import js7.data.order.{Order, OrderId}
import js7.data.workflow.WorkflowPath
import js7.data_for_java.agent.{JAgentRef, JAgentRefState}
import js7.data_for_java.cluster.JClusterState
import js7.data_for_java.common.JJournaledState
import js7.data_for_java.item.{JInventoryItem, JRepo}
import js7.data_for_java.lock.{JLock, JLockState}
import js7.data_for_java.order.JOrder
import js7.data_for_java.order.JOrderPredicates.any
import js7.data_for_java.vavr.VavrConverters._
import js7.data_for_java.workflow.{JWorkflow, JWorkflowId}
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._
import scala.jdk.StreamConverters._

@javaApi
final case class JControllerState(asScala: ControllerState)
extends JJournaledState[JControllerState, ControllerState]
{
  @Nonnull
  def eventId: Long =
    asScala.eventId

  @Nonnull
  def clusterState: JClusterState =
    JClusterState(asScala.clusterState)

  @Nonnull
  @Deprecated
  @deprecated("Please use repo.idToWorkflow", "2021-02-22")
  def idToWorkflow(@Nonnull workflowId: JWorkflowId): VEither[Problem, JWorkflow] =
    repo.idToWorkflow(workflowId)

  @Nonnull
  @Deprecated
  @deprecated("Please use repo.pathToWorkflow", "2021-02-22")
  def pathToWorkflow(@Nonnull workflowPath: WorkflowPath): VEither[Problem, JWorkflow] =
    repo.pathToWorkflow(workflowPath)

  @Nonnull
  def repo: JRepo =
    new JRepo(asScala.repo)

  /** Looks up an AgentRef VersionedItem in the current version. */
  @Nonnull
  def idToAgentRef(@Nonnull id: AgentId): VEither[Problem, JAgentRef] =
    asScala.idToAgentRefState.checked(id)
      .map(_.agentRef)
      .map(JAgentRef.apply)
      .toVavr

  /** Looks up an AgentRef VersionedItem in the current version. */
  @Deprecated
  @deprecated("Use idToAgentRef", "2020-12-11")
  @Nonnull
  def nameToAgentRef(@Nonnull id: AgentId): VEither[Problem, JAgentRef] =
    idToAgentRef(id)

  /** Looks up an AgentRefState. */
  @Nonnull
  def idToAgentRefState(@Nonnull agentId: AgentId): VEither[Problem, JAgentRefState] =
    asScala.idToAgentRefState.checked(agentId)
      .map(JAgentRefState.apply)
      .toVavr

  /** Looks up an AgentRef VersionedItem in the current version. */
  @Nonnull
  def idToLock(@Nonnull id: LockId): VEither[Problem, JLock] =
    asScala.idToLockState.checked(id)
      .map(_.lock)
      .map(JLock.apply)
      .toVavr

  /** Looks up a LockState. */
  @Nonnull
  def idToLockState(@Nonnull lockId: LockId): VEither[Problem, JLockState] =
    asScala.idToLockState.checked(lockId)
      .map(JLockState.apply)
      .toVavr

  @Nonnull
  def orderIds: java.util.Set[OrderId] =
    asScala.idToOrder.keySet.asJava

  @Nonnull
  def idToOrder(@Nonnull orderId: OrderId): java.util.Optional[JOrder] =
    asScala.idToOrder.get(orderId)
      .map(JOrder.apply)
      .toJava

  /** Looks up an OrderId and returns a Left(Problem) if the OrderId is unknown. */
  @Nonnull
  def idToCheckedOrder(@Nonnull orderId: OrderId): VEither[Problem, JOrder] =
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

  @Nonnull
  def ordersBy(@Nonnull predicate: Order[Order.State] => Boolean): java.util.stream.Stream[JOrder] =
    asScala.idToOrder
      .valuesIterator
      .filter(predicate)
      .map(JOrder.apply)
      .asJavaSeqStream

  @Nonnull
  def orderIsInCurrentVersionWorkflow: Order[Order.State] => Boolean =
    _.workflowId.versionId == asScala.repo.versionId

  @Nonnull
  def orderStateToCount(): java.util.Map[Class[_ <: Order.State], java.lang.Integer] =
    orderStateToCount(any)

  @Nonnull
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

  /** Includes the type. */
  def inventoryItemToJson(item: JInventoryItem): String =
    ControllerState.inventoryItemJsonCodec(item.asScala).compactPrint
}
