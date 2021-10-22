package js7.data.board

import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
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
    board +: Observable.fromIterable(notices.map(Notice.Snapshot(board.path, _)))
    // NoticeExpectation are recovered from Order[Order.ExpectingNotice]
  }

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

  def deleteNotice(noticeId: NoticeId): Checked[BoardState] =
    for {
      notice <- idToNotice.checked(noticeId)
      _ <- notice match {
        case _: NoticeExpectation => Left(Problem("NoticeExpectation is not deletable"))
        case _: Notice => Right(())
      }
    } yield
      copy(idToNotice = idToNotice - noticeId)
}
