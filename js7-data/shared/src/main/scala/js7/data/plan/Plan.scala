package js7.data.plan

import fs2.Stream
import js7.base.circeutils.CirceUtils.deriveCodecWithDefaults
import js7.base.circeutils.typed.Subtype
import js7.base.problem.Checked
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.StandardMapView
import js7.data.board.NoticeEvent.NoticeDeleted
import js7.data.board.{BoardNoticeKey, BoardPath, Notice, NoticeId, NoticeSnapshot, PlannedBoard}
import js7.data.event.KeyedEvent
import js7.data.order.OrderId
import js7.data.plan.Plan.Snapshot
import org.jetbrains.annotations.TestOnly
import scala.collection.{MapView, View}

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

  export id.planKey

  def estimatedSnapshotSize: Int =
    toPlannedBoard.values.view.map(_.estimatedSnapshotSize).sum

  def toSnapshotStream: Stream[fs2.Pure, Snapshot | NoticeSnapshot] =
    Stream.iterable(snapshot) ++
      Stream.iterable(toPlannedBoard.values).flatMap(_.toSnapshotStream)

  def toStringStream: Stream[fs2.Pure, String] =
    Stream.emit(s"$id${isClosed ?? " CLOSED"}${isDead ?? " ❌DEAD"}") ++
      Stream.iterable(orderIds.toVector.sorted)
        .append:
          Stream.iterable(toPlannedBoard.values.toVector.sorted).flatMap(_.toStringStream)
        .map(o => s"  $o")

  override def toString =
    s"Plan($id${isClosed ?? " CLOSED"} {${orderIds.toVector.sorted.mkString(" ")}} ${
      toPlannedBoard.values.toVector.sortBy(_.boardPath).mkString(", ")
    })"

  private def snapshot: Option[Snapshot] =
    isClosed ? Snapshot(id, isClosed = isClosed)

  def recoverSnapshot(snapshot: Snapshot): Plan =
    copy(isClosed = snapshot.isClosed)

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

  def plannedBoard(boardPath: BoardPath): PlannedBoard =
    toPlannedBoard.getOrElse(boardPath, PlannedBoard(id / boardPath))

  def removeBoard(boardPath: BoardPath): Option[Plan] =
    val plan = copy(toPlannedBoard = toPlannedBoard - boardPath)
    !plan.isEmpty ? plan

  def deadNoticeDeleted: View[KeyedEvent[NoticeDeleted]] =
    if !isDead then
      View.empty
    else
      noticeIds.map: noticeId =>
        noticeId.boardPath <-: NoticeDeleted(noticeId.plannedNoticeKey)

  def toNotice: MapView[BoardNoticeKey, Notice] =
    new StandardMapView[BoardNoticeKey, Notice]:
      override def keySet: Set[BoardNoticeKey] =
        toPlannedBoard.values.view.flatMap(_.notices).map(_.id.boardNoticeKey).toSet

      def get(boardNoticeKey: BoardNoticeKey): Option[Notice] =
        toPlannedBoard.get(boardNoticeKey.boardPath).flatMap: plannedBoard =>
          plannedBoard.maybeNotice(boardNoticeKey.noticeKey)

  private def noticeIds: View[NoticeId] =
    toPlannedBoard.values.view.flatMap: plannedBoard =>
      plannedBoard.noticeKeys.view.map: noticeKey =>
        id / plannedBoard.boardPath / noticeKey

  /** A dead Plan will be deleted immediately and should not exist. */
  private def isDead: Boolean =
    isClosed && orderIds.isEmpty

  def isEmpty: Boolean =
    orderIds.isEmpty && toPlannedBoard.isEmpty

  def hasOrders: Boolean =
    orderIds.nonEmpty

  def isNoticeAnnounced(boardNoticeKey: BoardNoticeKey): Boolean =
    toPlannedBoard.get(boardNoticeKey.boardPath).exists(_.isAnnounced(boardNoticeKey.noticeKey))


object Plan:

  @TestOnly @throws[RuntimeException]
  def apply(
    planId: PlanId,
    orderIds: Set[OrderId] = Set.empty,
    plannedBoards: Iterable[PlannedBoard] = Nil,
    isClosed: Boolean)
  : Plan =
    new Plan(planId, orderIds, plannedBoards.toKeyedMap(_.boardPath), isClosed = isClosed)

  def checked(
    planId: PlanId,
    orderIds: Set[OrderId] = Set.empty,
    plannedBoards: Iterable[PlannedBoard] = Nil,
    isClosed: Boolean)
  : Checked[Plan] =
    planId.checked.map: _ =>
      new Plan(planId, orderIds, plannedBoards.toKeyedMap(_.boardPath), isClosed = isClosed)

  def initial(planId: PlanId): Plan =
    new Plan(planId, Set.empty, Map.empty, false)


  given Ordering[Plan] = Ordering.by(_.id)


  final case class Snapshot(planId: PlanId, isClosed: Boolean = false):
    export planId.{planSchemaId, planKey}

    override def productPrefix = s"Plan.Snapshot"

  val subtype: Subtype[Snapshot] =
    Subtype.named[Snapshot](deriveCodecWithDefaults, "Plan")
