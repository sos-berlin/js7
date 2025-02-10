package js7.data.board

import fs2.Stream
import js7.base.circeutils.CirceUtils.deriveRenamingCodec
import js7.base.circeutils.typed.Subtype
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.utils.CatsUtils.Nel
import js7.base.utils.CatsUtils.syntax.mkString
import js7.base.utils.MultipleLinesBracket.Square
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isStrict
import js7.base.utils.{MultipleLinesBracket, Tests}
import js7.data.board.BoardState.*
import js7.data.item.UnsignedSimpleItemState
import js7.data.order.OrderId
import js7.data.plan.PlanSchemaId

final case class BoardState(
  board: BoardItem,
  orderToConsumptionStack: Map[OrderId, Nel[PlannedNoticeKey]] = Map.empty)
extends UnsignedSimpleItemState:

  protected type Self = BoardState
  val companion: BoardState.type = BoardState

  val item: BoardItem = board
  def path: BoardPath = item.path

  export item.isGlobal

  def updateItem(item: BoardItem): Checked[BoardState] =
    Right(copy(board = item))

  override def toString =
    s"BoardState(${board.pathRev} orderToConsumptionStack=(${
      orderToConsumptionStack.toArray.sortBy(_._1).view.map((k, v) => s"$k -> $v").mkString(", ")
    }))"

  override def toSnapshotStream: Stream[fs2.Pure, BoardItem | NoticeConsumptionSnapshot] =
    Stream.emit(board) ++
      Stream.iterable(orderToConsumptionStack).map: (orderId, consumptionStack) =>
        NoticeConsumptionSnapshot(path, orderId, consumptionStack)

  override def toStringStream: Stream[fs2.Pure, String] =
    Stream.emit(s"BoardState($path)") ++
      Stream.iterable(orderToConsumptionStack.toVector.sortBy(_._1)).map: (orderId, noticeKeys) =>
        s"  $orderId: ${noticeKeys.mkString(" ")}"

  def recoverConsumption(snapshot: NoticeConsumptionSnapshot): BoardState =
    copy(
      orderToConsumptionStack = orderToConsumptionStack
        .updated(snapshot.orderId, snapshot.plannedNoticeKeyStack))

  def pushConsumption(orderId: OrderId, plannedNoticeKey: PlannedNoticeKey): BoardState =
    // We can consume a non-existent PlannedNoticeKey, too, due to BoardExpression's or-operator
    val consumptionStack = orderToConsumptionStack.get(orderId).fold_(Nil, _.toList)
    copy(
      orderToConsumptionStack = orderToConsumptionStack.updated(orderId,
        Nel(plannedNoticeKey, consumptionStack)))

  def popConsumption(orderId: OrderId): Checked[(BoardState, PlannedNoticeKey)] =
    orderToConsumptionStack.checked(orderId).map: consumptions =>
      val Nel(plannedNoticeKey, remainingConsumptions) = consumptions
      val boardState = copy(
        orderToConsumptionStack =
          Nel.fromList(remainingConsumptions) match
            case None => orderToConsumptionStack - orderId
            case Some(nel) => orderToConsumptionStack.updated(orderId, nel))
      (boardState, plannedNoticeKey)

  private def requireNoNoticePlaceIsInUse(
    planSchemaId: PlanSchemaId,
    noticePlaces: Iterable[(PlannedNoticeKey, NoticePlace)])
  : Unit =
    var msg = ""
    noticePlaces.filter(_._2.isInUse).foreachWithBracket(Square):
      case ((key, noticePlace), br) =>
        msg = s"Remove $planSchemaId: Internal problem: Removing a being expected or consumed $key -> $noticePlace"
        logger.error(s"$br$msg")
    if isStrict && msg.nonEmpty then throw new AssertionError(msg)


object BoardState
extends UnsignedSimpleItemState.Companion[BoardState]/*
with EventDriven.Companion[BoardState, BoardEvent]*/:

  type Key = BoardPath
  type Item = BoardItem
  override type ItemState = BoardState

  private val logger = Logger[this.type]

  final case class NoticeConsumptionSnapshot(
    boardPath: BoardPath,
    orderId: OrderId,
    plannedNoticeKeyStack: Nel[PlannedNoticeKey])
  extends NoticeSnapshot:
    def plannedBoardId: PlannedBoardId =
      plannedNoticeKeyStack.head.planId / boardPath

  object NoticeConsumptionSnapshot:
    val subtype: Subtype[NoticeConsumptionSnapshot] =
      Subtype.named(
        deriveRenamingCodec[NoticeConsumptionSnapshot](Map(
          "noticeIdStack" -> "plannedNoticeKeyStack")),
        "NoticeConsumption")
