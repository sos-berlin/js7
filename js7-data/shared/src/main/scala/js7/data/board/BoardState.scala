package js7.data.board

import cats.effect.IO
import cats.syntax.option.*
import fs2.Stream
import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.Subtype
import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.utils.CatsUtils.Nel
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

final case class BoardState(
  board: BoardItem,
  idToNotice: Map[PlannedNoticeKey, NoticePlace] = Map.empty,
  orderToConsumptionStack: Map[OrderId, Nel[PlannedNoticeKey]] = Map.empty)
extends UnsignedSimpleItemState:

  protected type Self = BoardState
  val companion: BoardState.type = BoardState

  val item: BoardItem = board
  def path: BoardPath = item.path

  def updateItem(item: BoardItem): Checked[BoardState] =
    Right(copy(board = item))

  override def toString =
    s"BoardState(${board.pathRev} ${idToNotice.values.toVector.sorted.mkString(" ")})"

  override def toSnapshotStream
  : Stream[IO, BoardItem | Notice | NoticePlace.Snapshot | NoticeConsumptionSnapshot] =
    // Notice expectations are recovered from Order[Order.ExpectingNotice]
    Stream.iterable:
      View(board) ++
        notices ++
        idToNotice.values.view.flatMap(_.toSnapshot(path)) ++
        orderToConsumptionStack.view
          .map: (orderId, consumptionStack) =>
            NoticeConsumptionSnapshot(path, orderId, consumptionStack)

  def recover(snapshot: NoticeSnapshot): Checked[BoardState] =
    snapshot match
      case notice: Notice =>
        addNotice(notice)

      case snapshot: BoardState.NoticeConsumptionSnapshot =>
        Right(copy(
          orderToConsumptionStack = orderToConsumptionStack
            .updated(snapshot.orderId, snapshot.noticeIdStack)))

      case snapshot: NoticePlace.Snapshot =>
        Right(copy(
          idToNotice = idToNotice.updated(snapshot.noticeId,
            idToNotice.getOrElse(snapshot.noticeId, NoticePlace(snapshot.noticeId))
              .withSnapshot(snapshot))))

  def isAnnounced(noticeId: PlannedNoticeKey): Boolean =
    idToNotice.get(noticeId).exists(_.isAnnounced)

  def announceNotice(noticeId: PlannedNoticeKey): Checked[BoardState] =
    Right:
      updateNoticePlace:
        idToNotice.getOrElse(noticeId, NoticePlace(noticeId))
          .announce

  // COMPATIBLE with v2.3
  def addNoticeV2_3(notice: NoticeV2_3): Checked[BoardState] =
    addNotice(notice.toNotice(board.path))

  def addNotice(notice: Notice): Checked[BoardState] =
    Right:
      updateNoticePlace:
        idToNotice.getOrElse(notice.id, NoticePlace(notice.id))
          .post(notice)

  def addExpectation(noticeId: PlannedNoticeKey, orderId: OrderId): Checked[BoardState] =
    val noticePlace = idToNotice.getOrElse(noticeId, NoticePlace(noticeId))
    Right(updateNoticePlace:
      noticePlace.addExpecting(orderId))

  def removeExpectation(noticeId: PlannedNoticeKey, orderId: OrderId): Checked[BoardState] =
    Right:
      idToNotice.get(noticeId).fold(this) : noticePlace =>
        updateNoticePlace:
          noticePlace.removeExpecting(orderId)

  def startConsumption(noticeId: PlannedNoticeKey, orderId: OrderId): Checked[BoardState] =
    // We can consume a non-existent PlannedNoticeKey, too, due to BoardExpression's or-operator
    val noticePlace = idToNotice.getOrElse(noticeId, NoticePlace(noticeId))
    val consumptionStack = orderToConsumptionStack.get(orderId).fold_(Nil, _.toList)
    Right(copy(
      idToNotice = idToNotice.updated(noticeId, noticePlace.startConsumption(orderId)),
      orderToConsumptionStack = orderToConsumptionStack.updated(orderId,
        Nel(noticeId, consumptionStack))))

  def finishConsumption(orderId: OrderId, succeeded: Boolean): Checked[(BoardState, Option[PlannedNoticeKey])] =
    orderToConsumptionStack.get(orderId).fold(Checked(this -> none)): consumptions =>
      val Nel(noticeId, remainingConsumptions) = consumptions
      idToNotice.checked(noticeId).map: noticePlace =>
        val boardState =
          updateNoticePlace:
            noticePlace.finishConsumption(succeeded)
          .copy(
            orderToConsumptionStack =
              Nel.fromList(remainingConsumptions) match
                case None => orderToConsumptionStack - orderId
                case Some(nel) => orderToConsumptionStack.updated(orderId, nel))
        boardState -> noticeId.some

  /** @return L3.True: Notice exists<br>
    *         L3.False: Notice doesn't exist but is announced<br>
    *         L3.Unknown: Notice doesn't exist nor is it announced
    */
  def isNoticeAvailable(noticeId: PlannedNoticeKey): L3 =
    if containsNotice(noticeId) then
      L3.True
    else if isAnnounced(noticeId) then
      L3.False
    else
      L3.Unknown

  def containsNotice(noticeId: PlannedNoticeKey): Boolean =
    idToNotice.get(noticeId).exists(_.notice.isDefined)

  def containsNoticeKey(plannedNoticeKey: PlannedNoticeKey): Boolean =
    idToNotice.contains(plannedNoticeKey)

  def expectingOrders(noticeId: PlannedNoticeKey): Set[OrderId] =
    idToNotice.get(noticeId).fold_(Set.empty, _.expectingOrderIds)

  def notices: View[Notice] =
    idToNotice.values.view.flatMap(_.notice)

  def noticeCount: Int =
    idToNotice.values.count(_.notice.isDefined)

  def deleteNoticeEvent(noticeId: PlannedNoticeKey): Checked[KeyedEvent[NoticeDeleted]] =
    for _ <- checkDelete(noticeId) yield
      board.path <-: NoticeDeleted(noticeId)

  def removeNotice(noticeId: PlannedNoticeKey): Checked[BoardState] =
    for _ <- checkDelete(noticeId) yield
      idToNotice.get(noticeId).fold(this): noticePlace =>
        updateNoticePlace(noticePlace.removeNotice)

  /** PlanSchema must not contain an Order. */
  def removeNoticeKeysForPlanSchema(planSchemaId: PlanSchemaId): Option[BoardState] =
    val which = idToNotice.values.filter(_.noticeId.planId.planSchemaId == planSchemaId)
    which.foreachWithBracket(Square): (noticePlace, br) =>
      logger.trace(s"${br}Remove $planSchemaId: Remove $noticePlace")
    requireNoNoticePlaceIsInUse(planSchemaId, which)
    which.nonEmpty ?
      copy(idToNotice = idToNotice -- which.view.map(_.noticeId))

  private def requireNoNoticePlaceIsInUse(
    planSchemaId: PlanSchemaId,
    noticePlaces: Iterable[NoticePlace])
  : Unit =
    var msg = ""
    noticePlaces.filter(_.isInUse).foreachWithBracket(Square): (noticePlace, br) =>
      msg = s"Remove $planSchemaId: Internal problem: Removing a being expected or consumed $noticePlace"
      logger.error(s"${br}$msg")
    if isStrict && msg.nonEmpty then throw new AssertionError(msg)

  private def updateNoticePlace(noticePlace: NoticePlace): BoardState =
    copy(idToNotice =
      if noticePlace.isEmpty then
        idToNotice - noticePlace.noticeId
      else
        idToNotice.updated(noticePlace.noticeId, noticePlace))

  private def checkDelete(noticeId: PlannedNoticeKey): Checked[Unit] =
    for _ <- notice(noticeId) yield ()

  def notice(noticeId: PlannedNoticeKey): Checked[Notice] =
    for
      noticePlace <- idToNotice.checked(noticeId)
      notice <- noticePlace.notice match
        case None => Left(Problem(s"$noticeId does not denote a Notice (but a Notice expectation)"))
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
    noticeIdStack: Nel[PlannedNoticeKey])
  extends NoticeSnapshot

  object NoticeConsumptionSnapshot:
    val subtype: Subtype[NoticeConsumptionSnapshot] = Subtype.named(deriveCodec[NoticeConsumptionSnapshot], "NoticeConsumption")
