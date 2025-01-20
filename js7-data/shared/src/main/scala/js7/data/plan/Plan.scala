package js7.data.plan

import fs2.{Pure, Stream}
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.board.NoticeEvent.NoticeDeleted
import js7.data.board.{BoardNoticeKey, BoardPath, PlannedBoard, PlannedNoticeKey}
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

  def addNoticeKey(boardNoticeKey: BoardNoticeKey): Plan =
    if containsNoticeKey(boardNoticeKey) then
      this
    else
      copy(toPlannedBoard =
        toPlannedBoard.updatedWith(boardNoticeKey.boardPath): maybePlannedBoard =>
          val plannedBoard = maybePlannedBoard.getOrElse:
            PlannedBoard(id / boardNoticeKey.boardPath)
          Some(plannedBoard.addNoticeKey(boardNoticeKey.noticeKey)))

  def removeNoticeKey(boardNoticeKey: BoardNoticeKey): Option[Plan] =
    val plan =
      if !containsNoticeKey(boardNoticeKey) then
        this
      else
        copy(toPlannedBoard =
          toPlannedBoard.updatedWith(boardNoticeKey.boardPath):
            _.flatMap: plannedBoard =>
              plannedBoard.deleteNoticeKey(boardNoticeKey.noticeKey))
    !plan.isEmpty ? plan

  def containsNoticeKey(boardNoticeKey: BoardNoticeKey): Boolean =
    toPlannedBoard
      .get(boardNoticeKey.boardPath)
      .exists:
        _.noticeKeys(boardNoticeKey.noticeKey)

  def removeBoard(boardPath: BoardPath): Option[Plan] =
    val plan = copy(toPlannedBoard = toPlannedBoard - boardPath)
    !plan.isEmpty ? plan

  def removeDeadNoticeIds: View[KeyedEvent[NoticeDeleted]] =
    if !isDead then
      View.empty
    else
      toPlannedBoard.values.view.flatMap: plannedBoard =>
        plannedBoard.noticeKeys.view.map: noticeKey =>
          plannedBoard.boardPath <-: NoticeDeleted(PlannedNoticeKey(id, noticeKey))

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
