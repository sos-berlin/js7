package js7.data.plan

import fs2.{Pure, Stream}
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.board.NoticeEvent.NoticeDeleted
import js7.data.board.{BoardPath, NoticeKey, NoticePlace, PlannedBoard}
import js7.data.event.KeyedEvent
import js7.data.order.OrderId
import scala.collection.View

/** Plan, mirrors OrderIds and NoticeKeys that belong to this PlanId.
  *
  *  Each pair of PlanSchemaId and PlanKey (usually a day) has its own Plan.
  *
  *  Plan consists only of duplicated data whose originals are stored in:
  *    Order#planId, BoardState
  */
final case class Plan(
  id: PlanId,
  orderIds: Set[OrderId],
  toPlannedBoard: Map[BoardPath, PlannedBoard],
  isClosed: Boolean):

  def addOrders(orderIds: Iterable[OrderId]): Plan =
    if orderIds.isEmpty then
      this
    else
      copy(orderIds = this.orderIds ++ orderIds)

  def removeOrders(orderIds: Iterable[OrderId]): Plan =
    if orderIds.isEmpty then
      this
    else
      copy(orderIds = this.orderIds -- orderIds)

  def updateNoticePlace(boardPath: BoardPath, noticePlace: NoticePlace): Plan =
    copy(toPlannedBoard =
      toPlannedBoard.updatedWith(boardPath): maybePlannedBoard =>
        val plannedBoard = maybePlannedBoard.getOrElse:
          PlannedBoard(noticePlace.planId / boardPath)
        Some(plannedBoard.updateNoticePlace(noticePlace)))

  def deleteNoticePlace(boardPath: BoardPath, noticeKey: NoticeKey): Option[Plan] =
    val plan = copy(toPlannedBoard =
      toPlannedBoard.updatedWith(boardPath):
        _.flatMap: plannedBoard =>
          plannedBoard.deleteNoticePlace(noticeKey))
    !plan.isEmpty ? plan

  def deleteBoard(boardPath: BoardPath): Option[Plan] =
    val plan = copy(toPlannedBoard = toPlannedBoard - boardPath)
    !plan.isEmpty ? plan

  def deleteDeadNoticeIds: View[KeyedEvent[NoticeDeleted]] =
    if !isDead then
      View.empty
    else
      toPlannedBoard.values.view.flatMap: plannedBoard =>
        plannedBoard.keyToNoticePlace.values.view.map: noticePlace =>
          plannedBoard.boardPath <-: NoticeDeleted(noticePlace.noticeId)

  def isDead: Boolean =
    isClosed && orderIds.isEmpty

  def isEmpty: Boolean =
    orderIds.isEmpty && toPlannedBoard.isEmpty

  def hasOrders: Boolean =
    orderIds.nonEmpty

  def toStringStream: Stream[Pure, String] =
    Stream.emit(s"$id${isClosed ?? " CLOSED"}${isDead ?? " ❌DEAD"}") ++
      Stream.iterable(orderIds.toVector.sorted).append:
        Stream.iterable(toPlannedBoard.values.toVector.sorted)
      .map(o => s"  $o")

  override def toString =
    s"Plan($id${isClosed ?? " CLOSED"} {${orderIds.toVector.sorted.mkString(" ")}} ${
      toPlannedBoard.values.toVector.sortBy(_.boardPath).mkString(", ")})"


object Plan:

  def apply(
    planId: PlanId,
    orderIds: Set[OrderId] = Set.empty,
    plannedBoards: Iterable[PlannedBoard] = Nil,
    isClosed: Boolean)
  : Plan =
    new Plan(planId, orderIds, plannedBoards.toKeyedMap(_.boardPath), isClosed = isClosed)

  given Ordering[Plan] = Ordering.by(_.id)
