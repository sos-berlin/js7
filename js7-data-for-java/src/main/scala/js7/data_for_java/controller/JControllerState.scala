package js7.data_for_java.controller

import io.vavr.control.{Either => VEither}
import java.time.Instant
import java.util.{Map => JMap, Optional => JOptional}
import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.problem.Problem
import js7.base.time.JavaTimeConverters.AsScalaInstant
import js7.base.time.WallClock
import js7.base.web.Uri
import js7.data.agent.{AgentPath, AgentRef, AgentRefState}
import js7.data.board.{Board, BoardPath, BoardState}
import js7.data.calendar.{Calendar, CalendarPath}
import js7.data.controller.ControllerState
import js7.data.event.EventId
import js7.data.execution.workflow.instructions.InstructionExecutorService
import js7.data.item.{InventoryItem, InventoryItemKey}
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.lock.{Lock, LockPath, LockState}
import js7.data.order.{Order, OrderId, OrderObstacleCalculator}
import js7.data.orderwatch.{FileWatch, OrderWatchPath}
import js7.data.subagent.{SubagentId, SubagentItem, SubagentItemState, SubagentSelection, SubagentSelectionId}
import js7.data.value.Value
import js7.data_for_java.agent.{JAgentRef, JAgentRefState}
import js7.data_for_java.board.{JBoard, JBoardState}
import js7.data_for_java.calendar.JCalendar
import js7.data_for_java.cluster.JClusterState
import js7.data_for_java.common.JJournaledState
import js7.data_for_java.common.MoreJavaConverters.MapViewHasAsJava
import js7.data_for_java.item.{JInventoryItem, JRepo}
import js7.data_for_java.jobresource.JJobResource
import js7.data_for_java.lock.{JLock, JLockState}
import js7.data_for_java.order.JOrderPredicates.any
import js7.data_for_java.order.{JOrder, JOrderObstacle}
import js7.data_for_java.orderwatch.JFileWatch
import js7.data_for_java.subagent.{JSubagentItem, JSubagentItemState, JSubagentSelection}
import js7.data_for_java.vavr.VavrConverters._
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
  def instant: Instant =
    Instant.ofEpochMilli(EventId.toEpochMilli(asScala.eventId))

  @Nonnull
  def clusterState: JClusterState =
    JClusterState(asScala.clusterState)

  @Nonnull
  def repo: JRepo =
    new JRepo(asScala.repo)

  /** Looks up an AgentRef Item. */
  @Nonnull
  def pathToAgentRef: java.util.Map[AgentPath, JAgentRef] =
    asScala.pathTo(AgentRef)
      .mapValues(JAgentRef(_))
      .asJava

  /** Looks up the URI of an AgentPath.. */
  @Nonnull
  def agentToUri(@Nonnull agentPath: AgentPath): JOptional[Uri] =
    asScala.agentToUri(agentPath)
      .toJava

  /** Looks up an AgentRefState. */
  @Nonnull
  def pathToAgentRefState: java.util.Map[AgentPath, JAgentRefState] =
    asScala.pathTo(AgentRefState)
      .mapValues(JAgentRefState.apply)
      .asJava

  @Nonnull
  def idToSubagentItem: java.util.Map[SubagentId, JSubagentItem] =
    asScala.pathTo(SubagentItem)
      .mapValues(JSubagentItem(_))
      .asJava

  @Nonnull
  def idToSubagentItemState: java.util.Map[SubagentId, JSubagentItemState] =
    asScala.pathTo(SubagentItemState)
      .mapValues(JSubagentItemState(_))
      .asJava

  @Nonnull
  def idToSubagentSelection: java.util.Map[SubagentSelectionId, JSubagentSelection] =
    asScala.pathTo(SubagentSelection)
      .mapValues(JSubagentSelection(_))
      .asJava

  /** Looks up a Lock item in the current version. */
  @Nonnull
  def pathToLock: java.util.Map[LockPath, JLock] =
    asScala.keyTo(Lock)
      .mapValues(JLock.apply)
      .asJava

  /** Looks up a LockState. */
  @Nonnull
  def pathToLockState: java.util.Map[LockPath, JLockState] =
    asScala.pathTo(LockState)
      .mapValues(JLockState.apply)
      .asJava

  /** Looks up a Board item. */
  @Nonnull
  def pathToBoard: java.util.Map[BoardPath, JBoard] =
    keyToItem(Board, JBoard.apply)

  /** Looks up a BoardState. */
  @Nonnull
  def pathToBoardState: java.util.Map[BoardPath, JBoardState] =
    asScala.pathTo(BoardState)
      .view
      .mapValues(JBoardState.apply)
      .asJava

  @Nonnull
  def pathToCalendar: java.util.Map[CalendarPath, JCalendar] =
    keyToItem(Calendar, JCalendar.apply)

  /** Looks up a JFileWatch. */
  @Nonnull
  def pathToFileWatch: java.util.Map[OrderWatchPath, JFileWatch] =
    keyToItem(FileWatch, JFileWatch.apply)

  @deprecated("Use pathToFileWatch.values instead", "2.3")
  @Deprecated
  @Nonnull
  def fileWatches(): java.util.Collection[JFileWatch] =
    asScala.keyTo(FileWatch)
      .values
      .view
      .map(JFileWatch(_))
      .asJavaCollection

  /** Looks up a JJobResource. */
  @Nonnull
  def pathToJobResource: java.util.Map[JobResourcePath, JJobResource] =
    keyToItem(JobResource, JJobResource.apply)

  @Nonnull
  private def keyToItem[I <: InventoryItem, J](I: InventoryItem.Companion[I], toJava: I => J)
  : java.util.Map[I.Key, J] =
    asScala.keyTo(I)
      .view
      .mapValues(toJava)
      .asJava

  @Nonnull
  def deletionMarkedItems: java.util.Set[InventoryItemKey] =
    asScala.deletionMarkedItems.asJava

  @Nonnull
  def orderIds: java.util.Set[OrderId] =
    asScala.idToOrder.keySet.asJava

  @Nonnull
  def idToOrder: java.util.Map[OrderId, JOrder] =
    asScala.idToOrder
      .view
      .mapValues(JOrder.apply)
      .asJava

  /** Looks up an OrderId and returns a Left(Problem) if the OrderId is unknown. */
  @deprecated("Use idToOrder instread", "2.3.0")
  @Deprecated
  @Nonnull
  def idToCheckedOrder(@Nonnull orderId: OrderId): VEither[Problem, JOrder] =
    asScala.idToOrder.get(orderId)
      .map(JOrder.apply) match {
        case None => VEither.left(Problem(s"Unknown OrderId in JControllerState: ${orderId.string}"))
        case Some(o) => VEither.right(o)
      }

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
  def orderIsInCurrentVersionWorkflow: Order[Order.State] => Boolean =
    _.workflowId.versionId == asScala.repo.currentVersionId

  @Nonnull
  def orderStateToCount(): JMap[Class[_ <: Order.State], java.lang.Integer] =
    orderStateToCount(any)

  @Nonnull
  def orderStateToCount(predicate: Order[Order.State] => Boolean)
  : JMap[Class[_ <: Order.State], java.lang.Integer] =
    asScala.idToOrder.values.view
      .filter(predicate)
      .groupBy(_.state.getClass)
      .view.mapValues(o => java.lang.Integer.valueOf(o.size))
      .toMap.asJava

  @Nonnull
  def orderToObstacles(@Nonnull orderId: OrderId, @Nonnull now: Instant)
  : VEither[Problem, java.util.Set[JOrderObstacle]] = {
    val service = new InstructionExecutorService(
      WallClock.fixed(now.toTimestamp))
    orderObstacleCalculator
      .orderToObstacles(orderId)(service)
      .map(_.map(JOrderObstacle(_)).asJava)
      .toVavr
  }

  @Nonnull
  def ordersToObstacles(@Nonnull orderIds: java.lang.Iterable[OrderId], @Nonnull now: Instant)
  : VEither[Problem, java.util.Map[OrderId, java.util.Set[JOrderObstacle]]] =
    orderObstacleCalculator
      .ordersToObstacles(orderIds.asScala, now.toTimestamp)
      .map(_
        .map { case (id, obstacles) => id -> obstacles.map(JOrderObstacle(_)).asJava }
        .toMap.asJava)
      .toVavr

  @Nonnull
  def waitingForAdmissionOrderCount(now: Instant): Int =
    orderObstacleCalculator
      .waitingForAdmissionOrderCount(now.toTimestamp)

  private lazy val orderObstacleCalculator =
    new OrderObstacleCalculator(asScala)
}

object JControllerState extends JJournaledState.Companion[JControllerState, ControllerState]
{
  implicit val companion = this

  def apply(underlying: ControllerState) =
    new JControllerState(underlying)

  /** Includes the type. */
  def inventoryItemToJson(item: JInventoryItem): String =
    ControllerState.inventoryItemJsonCodec(item.asScala).compactPrint
}
