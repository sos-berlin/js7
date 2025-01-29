package js7.data_for_java.plan

import js7.base.annotation.javaApi
import js7.base.utils.Collections.implicits.RichIterable
import js7.data.board.{BoardPath, PlannedBoard}
import js7.data.order.OrderId
import js7.data.plan.{Plan, PlanId}
import js7.data_for_java.board.JPlannedBoard
import scala.jdk.CollectionConverters.*

final case class JPlan(
  asScala: Plan,
  private val toPlannedBoard_ : Map[BoardPath, JPlannedBoard]):

  def orderIds: java.util.Set[OrderId] =
    asScala.orderIds.asJava

  def isClosed: Boolean =
    asScala.isClosed

  def toPlannedBoard: java.util.Map[BoardPath, JPlannedBoard] =
    toPlannedBoard_.asJava


object JPlan:

  @javaApi
  def of(
    id: PlanId,
    orderIds: java.util.Set[OrderId],
    plannedBoards: java.lang.Iterable[JPlannedBoard],
    isClosed: Boolean)
  : JPlan =
    JPlan(
      Plan(
        id,
        orderIds.asScala.toSet,
        plannedBoards.asScala.map:
          jPlannedBoard => PlannedBoard(
            jPlannedBoard.id,
            jPlannedBoard.toNoticePlace.keySet().asScala.toSet),
        isClosed),
      plannedBoards.asScala.toKeyedMap(_.id.boardPath))
