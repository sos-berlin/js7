package js7.data_for_java.plan

import js7.base.annotation.javaApi
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.board.BoardPath
import js7.data.order.OrderId
import js7.data.plan.{Plan, PlanId}
import js7.data_for_java.board.JPlannedBoard
import js7.data_for_java.common.JavaWrapper
import js7.data_for_java.common.MoreJavaConverters.*
import scala.annotation.static
import scala.jdk.CollectionConverters.*

final case class JPlan(asScala: Plan) extends JavaWrapper:

  type AsScala = Plan

  def orderIds: java.util.Set[OrderId] =
    asScala.orderIds.asJava

  def state: Plan.Status =
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
    status: Plan.Status)
  : JPlan =
    JPlan:
      Plan.checked(
        id,
        status,
        orderIds.asScala.toSet,
        plannedBoards.asScala.view.map(_.asScala)
      ).orThrow

  @static val Open: Plan.Status = Plan.Status.Open
  @static val Closed: Plan.Status = Plan.Status.Closed
  @static val Finished: Plan.Status = Plan.Status.Finished
  @static val Deleted: Plan.Status = Plan.Status.Deleted
