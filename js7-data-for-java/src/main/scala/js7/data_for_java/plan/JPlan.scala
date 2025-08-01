package js7.data_for_java.plan

import js7.base.annotation.javaApi
import js7.base.utils.MoreJavaConverters.*
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.board.BoardPath
import js7.data.order.OrderId
import js7.data.plan.{Plan, PlanId, PlanStatus}
import js7.data_for_java.board.JPlannedBoard
import js7.data_for_java.common.JavaWrapper
import scala.jdk.CollectionConverters.*

final case class JPlan(asScala: Plan) extends JavaWrapper:

  type AsScala = Plan

  def orderIds: java.util.Set[OrderId] =
    asScala.orderIds.asJava

  @Deprecated @deprecated("Use `state` instead.", "2.7.3")
  def state: PlanStatus =
    asScala.status

  def status: PlanStatus =
    asScala.status

  /** Closed, Finished or Deleted. */
  def isClosed: Boolean =
    asScala.isClosed

  def toPlannedBoard: java.util.Map[BoardPath, JPlannedBoard] =
    asScala.toPlannedBoard.view.mapValues(JPlannedBoard(_)).asJava


object JPlan:

  @javaApi @throws[RuntimeException]
  def of(
    id: PlanId,
    orderIds: java.util.Set[OrderId],
    plannedBoards: java.lang.Iterable[JPlannedBoard],
    status: PlanStatus)
  : JPlan =
    JPlan:
      Plan.checked(
        id.nn,
        status.nn,
        orderIds.asScala.toSet,
        plannedBoards.asScala.view.map(_.asScala)
      ).orThrow
