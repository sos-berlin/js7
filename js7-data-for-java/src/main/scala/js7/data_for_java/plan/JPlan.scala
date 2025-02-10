package js7.data_for_java.plan

import js7.base.annotation.javaApi
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.board.BoardPath
import js7.data.order.OrderId
import js7.data.plan.{Plan, PlanId}
import js7.data_for_java.board.JPlannedBoard
import js7.data_for_java.common.JavaWrapper
import js7.data_for_java.common.MoreJavaConverters.*
import scala.jdk.CollectionConverters.*

final case class JPlan(asScala: Plan) extends JavaWrapper:

  type AsScala = Plan

  def orderIds: java.util.Set[OrderId] =
    asScala.orderIds.asJava

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
    isClosed: Boolean)
  : JPlan =
    JPlan:
      Plan.checked(
        id,
        orderIds.asScala.toSet,
        plannedBoards.asScala.view.map(_.asScala),
        isClosed
      ).orThrow
