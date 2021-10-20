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
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.data.agent.AgentPath
import js7.data.board.{Board, BoardPath}
import js7.data.calendar.{Calendar, CalendarPath}
import js7.data.controller.ControllerState
import js7.data.event.EventId
import js7.data.execution.workflow.instructions.InstructionExecutorService
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.lock.{Lock, LockPath}
import js7.data.order.{Order, OrderId, OrderObstacleCalculator}
import js7.data.orderwatch.{FileWatch, OrderWatchPath}
import js7.data.value.Value
import js7.data.workflow.WorkflowPath
import js7.data_for_java.agent.{JAgentRef, JAgentRefState}
import js7.data_for_java.board.{JBoard, JBoardState}
import js7.data_for_java.calendar.JCalendar
import js7.data_for_java.cluster.JClusterState
import js7.data_for_java.common.JJournaledState
import js7.data_for_java.item.{JInventoryItem, JRepo}
import js7.data_for_java.jobresource.JJobResource
import js7.data_for_java.lock.{JLock, JLockState}
import js7.data_for_java.order.JOrderPredicates.any
import js7.data_for_java.order.{JOrder, JOrderObstacle}
import js7.data_for_java.orderwatch.JFileWatch
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
  def instant: Instant =
    Instant.ofEpochMilli(EventId.toEpochMilli(asScala.eventId))

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
  def pathToAgentRef(@Nonnull agentPath: AgentPath): VEither[Problem, JAgentRef] =
    asScala.pathToAgentRefState.checked(agentPath)
      .map(_.agentRef)
      .map(JAgentRef.apply)
      .toVavr

  /** Looks up an AgentRefState. */
  @Nonnull
  def pathToAgentRefState(@Nonnull agentPath: AgentPath): VEither[Problem, JAgentRefState] =
    asScala.pathToAgentRefState.checked(agentPath)
      .map(JAgentRefState.apply)
      .toVavr

  /** Looks up a Lock item in the current version. */
  @Nonnull
  def pathToLock(@Nonnull lockPath: LockPath): VEither[Problem, JLock] =
    asScala.keyTo(Lock)
      .checked(lockPath)
      .map(JLock.apply)
      .toVavr

  /** Looks up a LockState. */
  @Nonnull
  def pathToLockState(@Nonnull lockPath: LockPath): VEither[Problem, JLockState] =
    asScala.pathToLockState
      .checked(lockPath)
      .map(JLockState.apply)
      .toVavr

  /** Looks up a Board item. */
  @Nonnull
  def pathToBoard(@Nonnull boardPath: BoardPath): VEither[Problem, JBoard] =
    asScala.keyTo(Board)
      .checked(boardPath)
      .map(JBoard.apply)
      .toVavr

  /** Looks up a BoardState. */
  @Nonnull
  def pathToBoardState(@Nonnull boardPath: BoardPath): VEither[Problem, JBoardState] =
    asScala.pathToBoardState
      .checked(boardPath)
      .map(JBoardState.apply)
      .toVavr

  @Nonnull
  def pathToCalendar(@Nonnull boardPath: CalendarPath): VEither[Problem, JCalendar] =
    asScala.keyTo(Calendar)
      .checked(boardPath)
      .map(JCalendar.apply)
      .toVavr

  /** Looks up a JFileWatch. */
  @Nonnull
  def pathToFileWatch(@Nonnull path: OrderWatchPath): VEither[Problem, JFileWatch] =
    asScala.keyTo(FileWatch)
      .checked(path)
      .map(JFileWatch(_))
      .toVavr

  @Nonnull
  def fileWatches(): java.util.Collection[JFileWatch] =
    asScala.keyTo(FileWatch)
      .values
      .view
      .map(JFileWatch(_))
      .asJavaCollection

  /** Looks up a JJobResource. */
  @Nonnull
  def pathToJobResource(@Nonnull path: JobResourcePath): VEither[Problem, JJobResource] =
    asScala.keyTo(JobResource)
      .checked(path)
      .map(JJobResource(_))
      .toVavr

  @Nonnull
  def orderIds: java.util.Set[OrderId] =
    asScala.idToOrder.keySet.asJava

  @Nonnull
  def idToOrder(@Nonnull orderId: OrderId): JOptional[JOrder] =
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
  lazy val eagerIdToOrder: JMap[OrderId, JOrder] =
    asScala.idToOrder
      .view.values.map(JOrder.apply)
      .toKeyedMap(_.id)
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
  def orderIsInCurrentVersionWorkflow: Order[Order.State] => Boolean =
    _.workflowId.versionId == asScala.repo.versionId

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
    OrderObstacleCalculator.orderToObstacles(orderId, asScala)(service)
      .map(_.map(JOrderObstacle(_)).asJava)
      .toVavr
  }
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
