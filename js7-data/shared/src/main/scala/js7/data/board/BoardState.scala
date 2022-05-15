package js7.data.board

import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.data.board.BoardEvent.NoticeDeleted
import js7.data.event.KeyedEvent
import js7.data.item.UnsignedSimpleItemState
import js7.data.order.OrderId
import monix.reactive.Observable

final case class BoardState(
  board: Board,
  idToNotice: Map[NoticeId, NoticePlace] = Map.empty)
extends UnsignedSimpleItemState
{
  def path = board.path

  type Item = Board
  def item = board

  def toSnapshotObservable: Observable[Any] = {
    board +: Observable.fromIterable(notices)
    // NoticeExpectation are recovered from Order[Order.ExpectingNotice]
  }

  // COMPATIBLE with v2.3
  def addNoticeV2_3(notice: NoticeV2_3): Checked[BoardState] =
    addNotice(notice.toNotice(board.path))

  def addNotice(notice: Notice): Checked[BoardState] =
    Right(copy(
      idToNotice = idToNotice + (notice.id -> notice)))

  def addExpectation(orderId: OrderId, noticeId: NoticeId): Checked[BoardState] =
    idToNotice.get(noticeId) match {
      case None =>
        Right(copy(
          idToNotice = idToNotice +
            (noticeId -> NoticeExpectation(noticeId, Set(orderId)))))

      case Some(expectation: NoticeExpectation) =>
        Right(copy(
          idToNotice = idToNotice +
            (noticeId -> expectation.copy(
              orderIds = expectation.orderIds + orderId))))

      case Some(_: Notice) =>
        Left(Problem("BoardState.addExpectation despite notice has been posted"))
    }

  def removeExpectation(orderId: OrderId, noticeId: NoticeId): Checked[BoardState] =
    Right(
      idToNotice.get(noticeId) match {
        case Some(expectation: NoticeExpectation) =>
          val updatedExpectation = expectation.orderIds.filterNot(_ == orderId)
          if (updatedExpectation.isEmpty)
            copy(
              idToNotice = idToNotice - noticeId)
          else
            copy(
              idToNotice = idToNotice +
                (noticeId -> expectation.copy(
                  orderIds = expectation.orderIds - orderId)))

        case _ =>
          this
      })

  def expectingOrders(noticeId: NoticeId): Set[OrderId] =
    idToNotice.get(noticeId) match {
      case Some(NoticeExpectation(_, orderIds)) => orderIds
      case _ => Set.empty
    }

  def notices: Iterable[Notice] =
    idToNotice.values.view.collect { case o: Notice => o }

  def deleteNoticeEvent(noticeId: NoticeId): Checked[KeyedEvent[NoticeDeleted]] =
    for (_ <- checkDelete(noticeId)) yield
      board.path <-: NoticeDeleted(noticeId)

  def deleteNotice(noticeId: NoticeId): Checked[BoardState] =
    for (_ <- checkDelete(noticeId)) yield
      copy(idToNotice = idToNotice - noticeId)

  def checkDelete(noticeId: NoticeId): Checked[Unit] =
    for (_ <- notice(noticeId)) yield ()

  def notice(noticeId: NoticeId): Checked[Notice] =
    for {
      noticePlace <- idToNotice.checked(noticeId)
      notice <- noticePlace match {
        case _: NoticeExpectation =>
          Left(Problem(s"$noticeId does not denote a Notice (but a NoticeExpectation)"))
        case o: Notice => Right(o)
      }
    } yield notice
}
