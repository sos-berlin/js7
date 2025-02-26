package js7.data.plan

import fs2.Stream
import io.circe.Codec
import js7.base.circeutils.CirceUtils.{deriveCodecWithDefaults, enumCodec}
import js7.base.circeutils.typed.Subtype
import js7.base.fs2utils.StreamExtensions.+:
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isStrict
import js7.base.utils.{Assertions, StandardMapView}
import js7.data.board.NoticeEvent.NoticeDeleted
import js7.data.board.{BoardNoticeKey, BoardPath, Notice, NoticeId, NoticeSnapshot, PlannedBoard}
import js7.data.event.KeyedEvent
import js7.data.order.OrderId
import js7.data.plan.Plan.Status.{Closed, Deleted, Finished, Open}
import js7.data.plan.Plan.{Snapshot, Status}
import js7.data.plan.PlanEvent.{PlanClosed, PlanDeleted, PlanFinished, PlanOpened, PlanStatusEvent}
import org.jetbrains.annotations.TestOnly
import scala.collection.{MapView, View}
import scala.math.Ordered.orderingToOrdered

/** Plan, mirrors OrderIds and NoticeKeys that belong to this PlanId.
  *
  *  Each pair of PlanSchemaId and PlanKey (usually a day) has its own Plan.
  *
  *  Plan consists only of duplicated data whose originals are stored in:
  *    Order#planId, BoardState
  */
final case class Plan(
  id: PlanId,
  status: Status,
  orderIds: Set[OrderId],
  toPlannedBoard: Map[BoardPath, PlannedBoard]):

  export id.planKey

  def estimatedSnapshotSize: Int =
    if this == Plan.Global then
      0
    else
      1 + toPlannedBoard.values.view.map(_.estimatedSnapshotSize).sum

  def toSnapshotStream: Stream[fs2.Pure, Snapshot | NoticeSnapshot] =
    if this == Plan.Global then
      Stream.empty
    else
      Snapshot(id, status) +:
        Stream.iterable(toPlannedBoard.values).flatMap(_.toSnapshotStream)

  def toStringStream: Stream[fs2.Pure, String] =
    Stream.emit(s"$id $status") ++
      Stream.iterable(orderIds.toVector.sorted)
        .append:
          Stream.iterable(toPlannedBoard.values.toVector.sorted).flatMap(_.toStringStream)
        .map(o => s"  $o")

  override def toString =
    s"Plan($id $status · ${
      if orderIds.isEmpty then
        "no orders"
      else
        orderIds.mkStringLimited(3)
    } · ${
      if toPlannedBoard.isEmpty then
        "no boards"
      else
        toPlannedBoard.values.toVector.sortBy(_.boardPath).mkString(", ")
    })"

  def applyEvent(event: PlanEvent): Checked[Plan] =
    event match
      case PlanStatusEvent(status) =>
        checkStatusChange(status).map: _ =>
          copy(status = status)

  def addOrders(orderIds: Iterable[OrderId]): Checked[Plan] =
    if isFinished then
      Left(Problem(s"$id does not accept orders because it is $status"))
    else if orderIds.isEmpty then
      Right(this)
    else
      assertThat(status == Open)
      Right(copy(orderIds = this.orderIds ++ orderIds))

  def removeOrders(orderIds: Iterable[OrderId]): Plan =
    if orderIds.isEmpty then
      this
    else
      copy(orderIds = this.orderIds -- orderIds)

  def plannedBoard(boardPath: BoardPath): PlannedBoard =
    toPlannedBoard.getOrElse(boardPath, PlannedBoard(id / boardPath))

  def removeBoard(boardPath: BoardPath): Plan =
    copy(toPlannedBoard = toPlannedBoard - boardPath)

  def planStatusEvent(newStatus: Status): Checked[View[KeyedEvent[NoticeDeleted | PlanStatusEvent]]] =
    checkStatusChange(newStatus).map: _ =>
      if newStatus == status then
        View.empty
      else
        newStatus match
          case Open => View.Single(id <-: PlanOpened)
          case Closed => (id <-: PlanClosed) +: copy(status = Closed).maybePlanFinished
          case Finished => View.Single(id <-: PlanFinished)
          case Deleted => uncheckedPlanDeleted

  def maybePlanFinished: View[KeyedEvent[PlanFinished | NoticeDeleted | PlanDeleted]] =
    if status != Closed || orderIds.nonEmpty then
      View.empty
    else
      (id <-: PlanFinished) +: copy(status = Finished).uncheckedPlanDeleted

  private def checkStatusChange(newStatus: Status): Checked[Unit] =
    (this.status, newStatus) match
      case (Open, Closed) => Checked.unit
      case (Closed, Finished) => Checked.unit
      case (Closed, Deleted) => Left(Problem:
        s"$id cannot be deleted because it is not finished due ${orderIds.mkStringLimited(3)}")
      case (Finished, Deleted) => Checked.unit
      case (Closed | Finished | Deleted, Open) => Checked.unit
      case (a, b) =>
        if a != b then
          Left(Problem(s"Plan.Status cannot be changed from $a to $b"))
        else
          !isStrict !! Problem(s"$id is already $newStatus")

  def maybePlanDeleted: View[KeyedEvent[NoticeDeleted | PlanDeleted]] =
    if status == Finished then
      uncheckedPlanDeleted
    else
      View.empty

  private def uncheckedPlanDeleted: View[KeyedEvent[NoticeDeleted | PlanDeleted]] =
    noticeIds.map: noticeId =>
      noticeId.boardPath <-: NoticeDeleted(noticeId.plannedNoticeKey)
    :+ (id <-: PlanDeleted)

  /** NoticeIds of Notices (but not of NoticePlaces without Notice). */
  private def noticeIds: View[NoticeId] =
    toPlannedBoard.values.view.flatMap: plannedBoard =>
      plannedBoard.toNoticePlace.filter(_._2.notice.isDefined).map: (noticeKey, _) =>
        id / plannedBoard.boardPath / noticeKey

  def toNotice: MapView[BoardNoticeKey, Notice] =
    new StandardMapView[BoardNoticeKey, Notice]:
      override def keySet: Set[BoardNoticeKey] =
        toPlannedBoard.values.view.flatMap(_.notices).map(_.id.boardNoticeKey).toSet

      def get(boardNoticeKey: BoardNoticeKey): Option[Notice] =
        toPlannedBoard.get(boardNoticeKey.boardPath).flatMap: plannedBoard =>
          plannedBoard.maybeNotice(boardNoticeKey.noticeKey)

  def isRemovableCandidate: Boolean =
    (status == Open || status == Deleted) && isEmpty

  private def isEmpty: Boolean =
    orderIds.isEmpty && toPlannedBoard.isEmpty

  /** Closed, Finished or Deleted. */
  def isClosed: Boolean =
    status >= Closed

  /** Finished or Deleted. */
  def isFinished: Boolean =
    status >= Finished

  def hasOrders: Boolean =
    orderIds.nonEmpty

  def isNoticeAnnounced(boardNoticeKey: BoardNoticeKey): Boolean =
    toPlannedBoard.get(boardNoticeKey.boardPath).exists(_.isAnnounced(boardNoticeKey.noticeKey))


object Plan:

  val Global: Plan =
    Plan(PlanId.Global, Status.Open, Set.empty, Map.empty)

  @TestOnly @throws[RuntimeException]
  def apply(
    planId: PlanId,
    status: Status,
    orderIds: Set[OrderId] = Set.empty,
    plannedBoards: Iterable[PlannedBoard] = Nil)
  : Plan =
    new Plan(planId, status, orderIds, plannedBoards.toKeyedMap(_.boardPath))

  def checked(
    planId: PlanId,
    status: Status,
    orderIds: Set[OrderId] = Set.empty,
    plannedBoards: Iterable[PlannedBoard] = Nil)
  : Checked[Plan] =
    planId.checked.map: _ =>
      new Plan(planId, status, orderIds, plannedBoards.toKeyedMap(_.boardPath))

  def fromSnapshot(snapshot: Snapshot): Plan =
    Plan(snapshot.planId, snapshot.status, Set.empty, Map.empty)


  given Ordering[Plan] = Ordering.by(_.id)


  enum Status:
    /** Orders may be added. */
    case Open

    /** No externals Orders may be added.
      *
      * Internal orders may still be added via workflow instruction. */
    case Closed

    /** Like Closed, but has no Orders. */
    case Finished

    /** Plan is closed and deleted. */
    case Deleted

  object Status:
    given Codec[Status] = enumCodec(valueOf)
    given Ordering[Status] = Ordering.by(_.ordinal)

  final case class Snapshot(planId: PlanId, status: Status):
    export planId.{planSchemaId, planKey}

    override def productPrefix = s"Plan.Snapshot"

  val subtype: Subtype[Snapshot] =
    Subtype.named[Snapshot](deriveCodecWithDefaults, "Plan")
