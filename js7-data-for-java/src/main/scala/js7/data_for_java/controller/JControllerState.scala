package js7.data_for_java.controller

import io.vavr.control.Either as VEither
import java.time.Instant
import java.util.{Map as JMap, Optional as JOptional, Set as JSet}
import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.metering.CallMeter
import js7.base.problem.Problem
import js7.base.time.JavaTimeConverters.AsScalaInstant
import js7.base.time.WallClock
import js7.base.utils.MoreJavaConverters.asJava
import js7.base.utils.ScalaUtils.syntax.RichMapView
import js7.base.web.Uri
import js7.data.agent.{AgentPath, AgentRef, AgentRefState}
import js7.data.board.{BoardPath, BoardState, GlobalBoard, NoticeId}
import js7.data.calendar.{Calendar, CalendarPath}
import js7.data.controller.ControllerState
import js7.data.event.EventId
import js7.data.execution.workflow.instructions.InstructionExecutorService
import js7.data.item.{InventoryItem, InventoryItemKey}
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.lock.{Lock, LockPath, LockState}
import js7.data.order.{Order, OrderId, OrderObstacleCalculator}
import js7.data.orderwatch.{FileWatch, OrderWatchPath}
import js7.data.plan.{PlanId, PlanSchemaId, PlanSchemaState}
import js7.data.subagent.{SubagentBundle, SubagentBundleId, SubagentId, SubagentItem, SubagentItemState}
import js7.data.value.Value
import js7.data.workflow.WorkflowControlId.syntax.*
import js7.data.workflow.{WorkflowControl, WorkflowControlId, WorkflowPath, WorkflowPathControl, WorkflowPathControlPath}
import js7.data_for_java.agent.{JAgentRef, JAgentRefState}
import js7.data_for_java.board.{JBoardItem, JBoardState, JNotice}
import js7.data_for_java.calendar.JCalendar
import js7.data_for_java.cluster.JClusterState
import js7.data_for_java.common.JJournaledState
import js7.data_for_java.item.{JInventoryItem, JRepo}
import js7.data_for_java.jobresource.JJobResource
import js7.data_for_java.lock.{JLock, JLockState}
import js7.data_for_java.order.JOrderPredicates.any
import js7.data_for_java.order.{JOrder, JOrderObstacle}
import js7.data_for_java.orderwatch.JFileWatch
import js7.data_for_java.plan.{JPlan, JPlanSchemaState}
import js7.data_for_java.subagent.{JSubagentBundle, JSubagentItem, JSubagentItemState}
import js7.data_for_java.vavr.VavrConverters.*
import js7.data_for_java.workflow.{JWorkflowControl, JWorkflowControlId, JWorkflowId}
import scala.annotation.nowarn
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.jdk.StreamConverters.*

@javaApi
final case class JControllerState(asScala: ControllerState)
extends JJournaledState[JControllerState, ControllerState]:

  @Nonnull
  def eventId: Long =
    asScala.eventId

  @Nonnull
  def instant: Instant =
    Instant.ofEpochMilli(EventId.toEpochMilli(asScala.eventId))

  @Nonnull
  def clusterState: JClusterState =
    JClusterState(asScala.clusterState)

  @Nonnull
  def repo: JRepo =
    new JRepo(asScala.repo)

  /** Look up an AgentRef Item. */
  @Nonnull
  def pathToAgentRef: JMap[AgentPath, JAgentRef] =
    asScala.pathToUnsignedSimple(AgentRef)
      .mapValues(JAgentRef(_))
      .asJava

  /** Look up the URI of an AgentPath. */
  @deprecated("Use agentToUris", "2.6")
  @Deprecated
  @Nonnull
  def agentToUri(@Nonnull agentPath: AgentPath): JOptional[Uri] =
    Some((asScala.agentToUris(agentPath): @nowarn("msg=deprecated")).head)
      .toJava

  /** Look up the URIs of an AgentPath. */
  @Nonnull
  def agentToUris(@Nonnull agentPath: AgentPath): java.util.List[Uri] =
    asScala.agentToUris(agentPath)
      .toList
      .asJava

  /** Look up an AgentRefState. */
  @Nonnull
  def pathToAgentRefState: JMap[AgentPath, JAgentRefState] =
    asScala.keyTo(AgentRefState)
      .mapValues(JAgentRefState.apply)
      .asJava

  @Nonnull
  def idToSubagentItem: JMap[SubagentId, JSubagentItem] =
    asScala.pathToUnsignedSimple(SubagentItem)
      .mapValues(JSubagentItem(_))
      .asJava

  @Nonnull
  def idToSubagentItemState: JMap[SubagentId, JSubagentItemState] =
    asScala.keyTo(SubagentItemState)
      .mapValues(JSubagentItemState(_))
      .asJava

  @deprecated("Use idToSubagentBundle!", "v2.7.2")
  @Deprecated
  @Nonnull
  def idToSubagentSelection: JMap[SubagentBundleId, JSubagentBundle] =
    idToSubagentBundle

  @Nonnull
  def idToSubagentBundle: JMap[SubagentBundleId, JSubagentBundle] =
    asScala.pathToUnsignedSimple(SubagentBundle)
      .mapValues(JSubagentBundle(_))
      .asJava

  @Nonnull
  def idToPlanSchemaState: JMap[PlanSchemaId, JPlanSchemaState] =
    asScala.keyTo(PlanSchemaState).mapValues(JPlanSchemaState(_)).asJava

  /** Look up a Lock item in the current version. */
  @Nonnull
  def pathToLock: JMap[LockPath, JLock] =
    asScala.keyToItem(Lock)
      .mapValues(JLock.apply)
      .asJava

  /** Look up a LockState. */
  @Nonnull
  def pathToLockState: JMap[LockPath, JLockState] =
    asScala.keyTo(LockState)
      .mapValues(JLockState.apply)
      .asJava

  /** Look up a Board item. */
  @Nonnull
  def pathToBoard: JMap[BoardPath, JBoardItem] =
    asScala.keyToItem(GlobalBoard).mapValues(JBoardItem(_)).asJava

  /** Look up a GlobalBoard item. */
  @Nonnull
  def pathToBoardState: JMap[BoardPath, JBoardState] =
    asScala.keyTo(BoardState)
      .view
      .mapValues(JBoardState.apply)
      .asJava

  @Nonnull
  def orderToAvailableNotices(@Nonnull orderId: OrderId): java.util.List[JNotice] =
    asScala.orderToAvailableNotices(orderId)
      .map(JNotice(_))
      .asJava

  @Nonnull
  def orderToStillExpectedNotices(@Nonnull orderId: OrderId): java.util.List[NoticeId] =
    asScala.orderToStillExpectedNotices(orderId)
      .asJava

  @Nonnull
  def toPlanSchemaState: JMap[PlanSchemaId, JPlanSchemaState] =
    asScala.keyTo(PlanSchemaState).mapValues(JPlanSchemaState(_)).asJava

  /** Returns the requested Plan (even if it is not in memory). */
  @Nonnull
  def plan(@Nonnull planId: PlanId): VEither[Problem, JPlan] =
    asScala.plan(planId).map(JPlan(_)).toVavr

  @Nonnull
  def toPlan: JMap[PlanId, JPlan] =
    asScala.toPlan.mapValues(JPlan(_)).asJava

  @Nonnull
  def pathToCalendar: JMap[CalendarPath, JCalendar] =
    keyToItem(Calendar, JCalendar.apply)

  /** Look up a JFileWatch. */
  @Nonnull
  def pathToFileWatch: JMap[OrderWatchPath, JFileWatch] =
    keyToItem(FileWatch, JFileWatch.apply)

  /** Look up a JJobResource. */
  @Nonnull
  def pathToJobResource: JMap[JobResourcePath, JJobResource] =
    keyToItem(JobResource, JJobResource.apply)

  @Nonnull
  def idToWorkflowControl: JMap[JWorkflowControlId, JWorkflowControl] =
    asScala.keyToItem(WorkflowControl)
      .mapIsomorphic(JWorkflowControlId(_), JWorkflowControl(_))(_.asScala)
      .asJava

  @Nonnull
  private def keyToItem[I <: InventoryItem, J](I: InventoryItem.Companion[I], toJava: I => J)
  : JMap[I.Key, J] =
    asScala.keyToItem(I)
      .view
      .mapValues(toJava)
      .asJava

  @Nonnull
  def workflowPathControlToIgnorantAgent: JMap[WorkflowPath, JSet[AgentPath]] =
    asScala
      .itemToIgnorantAgents(WorkflowPathControl)
      .mapIsomorphic(_.workflowPath, _.asJava)(WorkflowPathControlPath(_))
      .asJava

  @Nonnull
  def workflowControlToIgnorantAgent: JMap[JWorkflowId, JSet[AgentPath]] =
    asScala
      .itemToIgnorantAgents(WorkflowControl)
      .mapIsomorphic(o => JWorkflowId(o.workflowId), _.asJava)(id => WorkflowControlId(id.asScala))
      .asJava

  @Nonnull
  def deletionMarkedItems: java.util.Set[InventoryItemKey] =
    asScala.deletionMarkedItems.asJava

  @Nonnull
  def orderIds: java.util.Set[OrderId] =
    asScala.idToOrder.keySet.asJava

  @Nonnull
  def idToOrder: JMap[OrderId, JOrder] =
    asScala.idToOrder
      .view
      .mapValues(JOrder.apply)
      .asJava

  @Nonnull
  def ordersBy(@Nonnull predicate: Order[Order.State] => Boolean)
  : java.util.stream.Stream[JOrder] =
    asScala.idToOrder
      .valuesIterator
      .filter(predicate)
      .map(JOrder.apply)
      .asJavaSeqStream

  /** The named values as seen at the current workflow position. */
  @Nonnull
  def orderNamedValues(@Nonnull orderId: OrderId): VEither[Problem, JMap[String, Value]] =
    asScala.orderNamedValues(orderId)
      .map(_.toMap.asJava)
      .toVavr

  @Nonnull
  def orderStateToCount(): JMap[Class[? <: Order.State], java.lang.Integer] =
    orderStateToCount(any)

  @Nonnull
  def orderStateToCount(@Nonnull predicate: Order[Order.State] => Boolean)
  : JMap[Class[? <: Order.State], java.lang.Integer] =
    asScala.idToOrder.values.view
      .filter(predicate)
      .groupBy(_.state.getClass)
      .view.mapValues(o => java.lang.Integer.valueOf(o.size))
      .toMap.asJava

  @Nonnull
  def orderToObstacles(@Nonnull orderId: OrderId, @Nonnull now: Instant)
  : VEither[Problem, java.util.Set[JOrderObstacle]] =
    val service = new InstructionExecutorService(
      WallClock.fixed(now.toTimestamp))
    orderObstacleCalculator
      .orderToObstacles(orderId)(using service)
      .map(_.map(JOrderObstacle(_)).asJava)
      .toVavr

  @Nonnull
  def ordersToObstacles(@Nonnull orderIds: java.lang.Iterable[OrderId], @Nonnull now: Instant)
  : VEither[Problem, JMap[OrderId, java.util.Set[JOrderObstacle]]] =
    orderObstacleCalculator
      .ordersToObstacles(orderIds.asScala, now.toTimestamp)
      .map(_
        .map { case (id, obstacles) => id -> obstacles.map(JOrderObstacle(_)).asJava }
        .toMap.asJava)
      .toVavr

  @Nonnull
  def waitingForAdmissionOrderCount(@Nonnull now: Instant): Int =
    orderObstacleCalculator
      .waitingForAdmissionOrderCount(now.toTimestamp)

  private def orderObstacleCalculator =
    new OrderObstacleCalculator(asScala)


object JControllerState extends JJournaledState.Companion[JControllerState, ControllerState]:
  implicit val companion: JJournaledState.Companion[JControllerState, ControllerState] =
    this

  def apply(underlying: ControllerState): JControllerState =
    new JControllerState(underlying)

  /** Includes the type. */
  def inventoryItemToJson(item: JInventoryItem): String =
    ControllerState.inventoryItemJsonCodec(item.asScala).compactPrint

  private val meterToPlanGet = CallMeter("JControllerState.toPlan.get")
  private val meterToPlanKeySet = CallMeter("JControllerState.toPlan.keySet")
