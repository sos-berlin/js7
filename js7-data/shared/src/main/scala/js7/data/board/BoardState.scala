package js7.data.board

import js7.base.problem.{Checked, Problem}
import js7.data.order.OrderId
import monix.reactive.Observable

final case class BoardState(
  board: Board,
  idToNotice: Map[NoticeId, NoticePlace] = Map.empty)
{
  def path = board.path

  def toSnapshotObservable: Observable[Any] = {
    board +: Observable.fromIterable(notices.map(Notice.Snapshot(board.path, _)))
    // NoticeExpectation are recovered from Order[Order.ExpectingNotice]
  }

  def addNotice(notice: Notice): BoardState =
    copy(
      idToNotice = idToNotice + (notice.id -> notice))

  def addExpectingOrder(orderId: OrderId, noticeId: NoticeId): Checked[BoardState] =
    idToNotice.get(noticeId) match {
      case None =>
        Right(copy(
          idToNotice = idToNotice +
            (noticeId -> NoticeExpectation(noticeId, orderId :: Nil))))

      case Some(expectation: NoticeExpectation) =>
        Right(copy(
          idToNotice = idToNotice +
            (noticeId -> expectation.copy(
              orderIds = expectation.orderIds.view.appended(orderId).toVector))))

      case Some(_: Notice) =>
        Left(Problem("BoardState.addExpectingOrder despite notice has been posted"))
    }

  def expectingOrders(noticeId: NoticeId): Seq[OrderId] =
    idToNotice.get(noticeId) match {
      case Some(NoticeExpectation(_, orderIds)) => orderIds
      case _ => Nil
    }

  def notices: Iterable[Notice] =
    idToNotice.values.view.collect { case o: Notice => o }

  def deleteNotice(noticeId: NoticeId): BoardState =
    copy(idToNotice = idToNotice - noticeId)
}
