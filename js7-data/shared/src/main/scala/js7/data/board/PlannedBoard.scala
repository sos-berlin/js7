package js7.data.board

import fs2.Stream
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.problem.{Checked, Problem}
import js7.base.time.Timestamp
import js7.base.utils.L3
import js7.base.utils.MultipleLinesBracket.Square
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isStrict
import js7.data.board
import js7.data.board.NoticeEvent.NoticeDeleted
import js7.data.board.PlannedBoard.*
import js7.data.event.KeyedEvent
import js7.data.order.OrderId
import js7.data.plan.PlanId
import scala.collection.View

/** PlannedBoard, mirrors NoticeIds whose originals are stored in BoardState.
  *
  * A PlannedBoard mirrors NoticeIds of a PlannableBoard or a GlobalBoard.
  */
final case class PlannedBoard(
  id: PlannedBoardId,
  toNoticePlace: Map[NoticeKey, NoticePlace]):

  def estimatedSnapshotSize: Int =
    noticeCount +
      toNoticePlace.values.view.map(_.estimatedSnapshotSize).sum

  def toSnapshotStream: Stream[fs2.Pure, NoticeSnapshot] =
    // Notice expectations are recovered from Order[Order.ExpectingNotice]
    Stream.iterable(toNoticePlace).flatMap: (k, v) =>
      v.toSnapshotStream(id / k)

  def toStringStream: Stream[fs2.Pure, String] =
    Stream.emit(s"$id") ++
      Stream.iterable(toNoticePlace.toVector.sortBy(_._1)).map((k, v) => s"  $k -> $v")

  def recoverNoticeSnapshot(snapshot: Notice | NoticePlace.Snapshot): Checked[PlannedBoard] =
    val noticeKey = snapshot match
      case o: Notice => o.noticeKey
      case o: NoticePlace.Snapshot => o.noticeId.noticeKey
    Right(copy(
      toNoticePlace = toNoticePlace.updated(noticeKey,
        noticePlace(noticeKey).recoverSnapshot(snapshot))))

  def isAnnounced(noticeKey: NoticeKey): Boolean =
    toNoticePlace.get(noticeKey).exists(_.isAnnounced)

  def announceNotice(noticeKey: NoticeKey): Checked[PlannedBoard] =
    Right:
      updateNoticePlace(noticeKey, noticePlace(noticeKey).announce)

  // COMPATIBLE with v2.3
  def addNoticeV2_3(notice: NoticeV2_3): Checked[PlannedBoard] =
    addNotice(notice.toNotice(boardPath))

  def addNotice(notice: Notice): Checked[PlannedBoard] =
    Right:
      updateNoticePlace(
        notice.noticeKey,
        noticePlace(notice.noticeKey).post(notice))

  def addExpectation(noticeKey: NoticeKey, orderId: OrderId): PlannedBoard =
    updateNoticePlace(noticeKey, noticePlace(noticeKey).addExpecting(orderId))

  def removeExpectation(noticeKey: NoticeKey, orderId: OrderId): Checked[PlannedBoard] =
    Right:
      toNoticePlace.get(noticeKey).fold(this): noticePlace =>
        updateNoticePlace(noticeKey, noticePlace.removeExpecting(orderId))

  def startConsumption(noticeKey: NoticeKey, orderId: OrderId): PlannedBoard =
    // We can consume a non-existent PlannedNoticeKey, too, due to BoardExpression's or-operator
    copy(
      toNoticePlace = toNoticePlace.updated(noticeKey,
        noticePlace(noticeKey).startConsumption(orderId)))

  def finishConsumption(noticeKey: NoticeKey, succeeded: Boolean): Checked[PlannedBoard] =
    toNoticePlace.checked(noticeKey).map: noticePlace =>
      updateNoticePlace(noticeKey, noticePlace.finishConsumption(succeeded))

  /** @return L3.True: Notice exists<br>
    *         L3.False: Notice doesn't exist but is announced<br>
    *         L3.Unknown: Notice doesn't exist nor is it announced
    */
  def isNoticeAvailable(noticeKey: NoticeKey): L3 =
    if hasNotice(noticeKey) then
      L3.True
    else if isAnnounced(noticeKey) then
      L3.False
    else
      L3.Unknown

  def hasNotice(noticeKey: NoticeKey): Boolean =
    toNoticePlace.get(noticeKey).exists(_.notice.isDefined)

  def containsNoticeKey(noticeKey: NoticeKey): Boolean =
    toNoticePlace.contains(noticeKey)

  def expectingOrders(noticeKey: NoticeKey): Set[OrderId] =
    toNoticePlace.get(noticeKey).fold_(Set.empty, _.expectingOrderIds)

  def notices: View[Notice] =
    toNoticePlace.values.view.flatMap(_.notice)

  def noticeCount: Int =
    toNoticePlace.values.count(_.notice.isDefined)

  def notice(noticeKey: NoticeKey): Checked[Notice] =
    for
      noticePlace <- toNoticePlace.checked(noticeKey)
      notice <- noticePlace.notice match
        case None => Left(Problem(s"${id / noticeKey} does not denote a Notice (but a Notice expectation)"))
        case Some(notice) => Right(notice)
    yield
      notice

  def noticePlace(noticeKey: NoticeKey): NoticePlace =
    toNoticePlace.getOrElse(noticeKey, NoticePlace.empty)

  def deleteNoticeEvent(noticeKey: NoticeKey): Checked[KeyedEvent[NoticeDeleted]] =
    checkNoticeIsDeletable(noticeKey).rightAs:
      boardPath <-: NoticeDeleted(id.planId / noticeKey)

  def removeNotice(noticeKey: NoticeKey): Checked[PlannedBoard] =
    checkNoticeIsDeletable(noticeKey).rightAs:
      toNoticePlace.get(noticeKey).fold(this): noticePlace =>
        updateNoticePlace(noticeKey, noticePlace.removeNotice)

  private def checkNoticeIsDeletable(noticeKey: NoticeKey): Checked[Unit] =
    toNoticePlace.checked(noticeKey).map(_ => ())

  /** For NoticeMoved event, delete the NoticeKey. */
  def moveFromNoticePlace(noticeKey: NoticeKey): Checked[PlannedBoard] =
    toNoticePlace.checked(noticeKey).flatMap: noticePlace =>
      Right(copy(
        toNoticePlace = toNoticePlace - noticeKey))

  /** For NoticeMoved event, insert the NoticeKey. */
  def moveToNoticePlace(
    noticeKey: NoticeKey,
    noticePlace: NoticePlace,
    endOfLife: Option[Timestamp])
  : Checked[PlannedBoard] =
    if toNoticePlace.contains(noticeKey) then
      Left(Problem.pure(s"$noticeKey exists already"))
    else
      val newNoticePlace = noticePlace.copy(
        notice = noticePlace.notice.map(_.copy(
          id = id.planId / boardPath / noticeKey,
          endOfLife = endOfLife)))
      Right(copy(
        toNoticePlace = toNoticePlace.updated(noticeKey, newNoticePlace)))

  def removeNoticeKeys: Option[PlannedBoard] =
    logger.traceCall:
      toNoticePlace.foreachWithBracket(Square):
        case ((key, noticePlace), br) =>
          logger.trace(s"${br}Remove $key -> $noticePlace")
      requireNoNoticePlaceIsInUse()
      toNoticePlace.nonEmpty ?
        copy(toNoticePlace = Map.empty)

  private def requireNoNoticePlaceIsInUse(): Unit =
    var msg = ""
    toNoticePlace.view.filter(_._2.isInUse).foreachWithBracket(Square):
      case ((noticeKey, noticePlace), br) =>
        msg = s"requireNoNoticePlaceIsInUse: Internal problem: Removing a being expected or consumed $noticeKey -> $noticePlace"
        logger.error(s"$br$msg")
    if isStrict && msg.nonEmpty then throw new AssertionError(msg)

  private def updateNoticePlace(noticeKey: NoticeKey, noticePlace: NoticePlace): PlannedBoard =
    copy(toNoticePlace =
      if noticePlace.isEmpty then
        toNoticePlace - noticeKey
      else
        toNoticePlace.updated(noticeKey, noticePlace))

  def maybeNotice(noticeKey: NoticeKey): Option[Notice] =
    toNoticePlace.get(noticeKey).flatMap(_.notice)

  def planId: PlanId =
    id.planId

  def boardPath: BoardPath =
    id.boardPath

  def isEmpty: Boolean =
    toNoticePlace.isEmpty

  override def toString =
    s"PlannedBoard($id ${
      toNoticePlace.toArray.sortBy(_._1).view.map((k, v) => s"$k -> $v").mkString(", ")
    })"


object PlannedBoard:

  private val logger = Logger[this.type]

  def apply(
    id: PlannedBoardId,
    toNoticePlace: Map[NoticeKey, NoticePlace] = Map.empty)
  : PlannedBoard =
    new PlannedBoard(id, toNoticePlace)

  given Ordering[PlannedBoard] = Ordering.by(_.id)
