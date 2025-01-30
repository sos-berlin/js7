package js7.data.board

import cats.effect.IO
import cats.syntax.option.*
import fs2.Stream
import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.CirceUtils
import js7.base.circeutils.CirceUtils.deriveRenamingCodec
import js7.base.circeutils.typed.Subtype
import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.time.Timestamp
import js7.base.utils.Assertions.strictly
import js7.base.utils.CatsUtils.Nel
import js7.base.utils.CatsUtils.syntax.mkString
import js7.base.utils.MultipleLinesBracket.Square
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isStrict
import js7.base.utils.{L3, MultipleLinesBracket, Tests}
import js7.data.board.BoardState.*
import js7.data.board.NoticeEvent.NoticeDeleted
import js7.data.event.KeyedEvent
import js7.data.item.UnsignedSimpleItemState
import js7.data.order.OrderId
import js7.data.plan.PlanSchemaId
import scala.collection.View
import scala.collection.immutable.ArraySeq

final case class BoardState(
  board: BoardItem,
  toNoticePlace: Map[PlannedNoticeKey, NoticePlace] = Map.empty,
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
    s"BoardState(${board.pathRev} ${toNoticePlace
      .to(ArraySeq)
      .sortBy(_._1)
      .view.map((k, v) => s"$k -> $v")
      .mkString(" ")
    })"

  override def toSnapshotStream: Stream[IO, BoardItem | NoticeSnapshot] =
    // Notice expectations are recovered from Order[Order.ExpectingNotice]
    Stream.iterable:
      View(board) ++
        notices ++
        toNoticePlace.view.flatMap((k, v) => v.toSnapshot(path / k)) ++
        orderToConsumptionStack.view
          .map: (orderId, consumptionStack) =>
            NoticeConsumptionSnapshot(path, orderId, consumptionStack)

  override def toStringStream: Stream[fs2.Pure, String] =
    Stream.emit(s"BoardState($path)") ++
      Stream.iterable(toNoticePlace.toVector.sortBy(_._1)).map((k, v) => s"  $k -> $v") ++
      Stream.iterable(orderToConsumptionStack.toVector.sortBy(_._1)).map: (orderId, noticeKeys) =>
        s"  $orderId: ${noticeKeys.mkString(" ")}"

  def recover(snapshot: NoticeSnapshot): Checked[BoardState] =
    snapshot match
      case notice: Notice =>
        addNotice(notice)

      case snapshot: BoardState.NoticeConsumptionSnapshot =>
        Right(copy(
          orderToConsumptionStack = orderToConsumptionStack
            .updated(snapshot.orderId, snapshot.plannedNoticeKeyStack)))

      case snapshot: NoticePlace.Snapshot =>
        Right(copy(
          toNoticePlace = toNoticePlace.updated(snapshot.plannedNoticeKey,
            toNoticePlace.getOrElse(snapshot.plannedNoticeKey, NoticePlace.empty)
              .withSnapshot(snapshot))))

  def isAnnounced(plannedNoticeKey: PlannedNoticeKey): Boolean =
    toNoticePlace.get(plannedNoticeKey).exists(_.isAnnounced)

  def announceNotice(plannedNoticeKey: PlannedNoticeKey): Checked[BoardState] =
    Right:
      updateNoticePlace(
        plannedNoticeKey,
        toNoticePlace.getOrElse(plannedNoticeKey, NoticePlace.empty)
          .announce)

  // COMPATIBLE with v2.3
  def addNoticeV2_3(notice: NoticeV2_3): Checked[BoardState] =
    addNotice(notice.toNotice(board.path))

  def addNotice(notice: Notice): Checked[BoardState] =
    Right:
      updateNoticePlace(
        notice.plannedNoticeKey,
        toNoticePlace.getOrElse(notice.plannedNoticeKey, NoticePlace.empty)
          .post(notice))

  def addExpectation(plannedNoticeKey: PlannedNoticeKey, orderId: OrderId): Checked[BoardState] =
    val noticePlace = toNoticePlace.getOrElse(plannedNoticeKey, NoticePlace.empty)
    Right(updateNoticePlace(
      plannedNoticeKey,
      noticePlace.addExpecting(orderId)))

  def removeExpectation(plannedNoticeKey: PlannedNoticeKey, orderId: OrderId): Checked[BoardState] =
    Right:
      toNoticePlace.get(plannedNoticeKey).fold(this) : noticePlace =>
        updateNoticePlace(
          plannedNoticeKey,
          noticePlace.removeExpecting(orderId))

  def startConsumption(plannedNoticeKey: PlannedNoticeKey, orderId: OrderId): Checked[BoardState] =
    // We can consume a non-existent PlannedNoticeKey, too, due to BoardExpression's or-operator
    val noticePlace = toNoticePlace.getOrElse(plannedNoticeKey, NoticePlace.empty)
    val consumptionStack = orderToConsumptionStack.get(orderId).fold_(Nil, _.toList)
    Right(copy(
      toNoticePlace = toNoticePlace.updated(plannedNoticeKey,
        noticePlace.startConsumption(orderId)),
      orderToConsumptionStack = orderToConsumptionStack.updated(orderId,
        Nel(plannedNoticeKey, consumptionStack))))

  def finishConsumption(orderId: OrderId, succeeded: Boolean)
  : Checked[(BoardState, Option[PlannedNoticeKey])] =
    orderToConsumptionStack.get(orderId).fold(Checked(this -> none)): consumptions =>
      val Nel(plannedNoticeKey, remainingConsumptions) = consumptions
      toNoticePlace.checked(plannedNoticeKey).map: noticePlace =>
        val boardState =
          updateNoticePlace(
            plannedNoticeKey,
            noticePlace.finishConsumption(succeeded))
          .copy(
            orderToConsumptionStack =
              Nel.fromList(remainingConsumptions) match
                case None => orderToConsumptionStack - orderId
                case Some(nel) => orderToConsumptionStack.updated(orderId, nel))
        boardState -> plannedNoticeKey.some

  /** @return L3.True: Notice exists<br>
    *         L3.False: Notice doesn't exist but is announced<br>
    *         L3.Unknown: Notice doesn't exist nor is it announced
    */
  def isNoticeAvailable(plannedNoticeKey: PlannedNoticeKey): L3 =
    if hasNotice(plannedNoticeKey) then
      L3.True
    else if isAnnounced(plannedNoticeKey) then
      L3.False
    else
      L3.Unknown

  def hasNotice(plannedNoticeKey: PlannedNoticeKey): Boolean =
    toNoticePlace.get(plannedNoticeKey).exists(_.notice.isDefined)

  def containsNoticeKey(plannedNoticeKey: PlannedNoticeKey): Boolean =
    toNoticePlace.contains(plannedNoticeKey)

  def expectingOrders(plannedNoticeKey: PlannedNoticeKey): Set[OrderId] =
    toNoticePlace.get(plannedNoticeKey).fold_(Set.empty, _.expectingOrderIds)

  def notices: View[Notice] =
    toNoticePlace.values.view.flatMap(_.notice)

  def noticeCount: Int =
    toNoticePlace.values.count(_.notice.isDefined)

  def deleteNoticeEvent(plannedNoticeKey: PlannedNoticeKey): Checked[KeyedEvent[NoticeDeleted]] =
    for _ <- checkDelete(plannedNoticeKey) yield
      board.path <-: NoticeDeleted(plannedNoticeKey)

  def removeNotice(plannedNoticeKey: PlannedNoticeKey): Checked[BoardState] =
    for _ <- checkDelete(plannedNoticeKey) yield
      toNoticePlace.get(plannedNoticeKey).fold(this): noticePlace =>
        updateNoticePlace(plannedNoticeKey, noticePlace.removeNotice)

  def moveNotice(
    plannedNoticeKey: PlannedNoticeKey,
    newPlannedNoticeKey: PlannedNoticeKey,
    endOfLife: Option[Timestamp])
  : Checked[BoardState] =
    toNoticePlace.checked(plannedNoticeKey).flatMap: noticePlace =>
      if toNoticePlace.contains(newPlannedNoticeKey) then
        Left(Problem.pure(s"$newPlannedNoticeKey exists already"))
      else
        val newNoticePlace = noticePlace.copy(
          notice = noticePlace.notice.map(_.copy(
            id = path / newPlannedNoticeKey,
            endOfLife = endOfLife)))
        Right(copy(
          toNoticePlace = toNoticePlace
            - plannedNoticeKey
            + (newPlannedNoticeKey -> newNoticePlace)))

  def removeNoticeKeysForPlanSchema(planSchemaId: PlanSchemaId): Option[BoardState] =
    strictly(orderToConsumptionStack.isEmpty)
    val which = toNoticePlace.filter(_._1.planSchemaId == planSchemaId)
    which.foreachWithBracket(Square):
      case ((key, noticePlace), br) =>
        logger.trace(s"${br}Remove $planSchemaId: Remove $key -> $noticePlace")
    requireNoNoticePlaceIsInUse(planSchemaId, which)
    which.nonEmpty ?
      copy(toNoticePlace = toNoticePlace -- which.keys)

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

  private def updateNoticePlace(key: PlannedNoticeKey, noticePlace: NoticePlace): BoardState =
    copy(toNoticePlace =
      if noticePlace.isEmpty then
        toNoticePlace - key
      else
        toNoticePlace.updated(key, noticePlace))

  private def checkDelete(plannedNoticeKey: PlannedNoticeKey): Checked[Unit] =
    for _ <- notice(plannedNoticeKey) yield ()

  def notice(plannedNoticeKey: PlannedNoticeKey): Checked[Notice] =
    for
      noticePlace <- toNoticePlace.checked(plannedNoticeKey)
      notice <- noticePlace.notice match
        case None => Left(Problem(s"$plannedNoticeKey does not denote a Notice (but a Notice expectation)"))
        case Some(notice) => Right(notice)
    yield
      notice


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
  extends NoticeSnapshot

  object NoticeConsumptionSnapshot:
    val subtype: Subtype[NoticeConsumptionSnapshot] =
      Subtype.named(
        deriveRenamingCodec[NoticeConsumptionSnapshot](Map(
          "noticeIdStack" -> "plannedNoticeKeyStack")),
        "NoticeConsumption")
