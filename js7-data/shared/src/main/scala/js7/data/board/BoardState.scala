package js7.data.board

import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax._
import js7.data.board.BoardEvent.NoticeDeleted
import js7.data.event.KeyedEvent
import js7.data.item.UnsignedSimpleItemState
import js7.data.order.OrderId
import monix.reactive.Observable
import scala.collection.View

final case class BoardState(
  board: Board,
  idToNotice: Map[NoticeId, NoticePlace] = Map.empty)
extends UnsignedSimpleItemState
{
  def path = board.path

  type Item = Board
  def item = board

  override def toString = s"BoardState(${board.pathRev} $idToNotice)"

  def toSnapshotObservable: Observable[Any] = {
    board +: Observable.fromIterable(notices)
    // NoticeExpectation are recovered from Order[Order.ExpectingNotice]
  }

  // COMPATIBLE with v2.3
  def addNoticeV2_3(notice: NoticeV2_3): Checked[BoardState] =
    addNotice(notice.toNotice(board.path))

  def addNotice(notice: Notice): Checked[BoardState] =
    idToNotice.get(notice.id) match {
      case None =>
        Right(copy(
          idToNotice = idToNotice.updated(
            notice.id,
            NoticePlace(Some(notice)))))

      case Some(noticePlace) =>
        Right(copy(
          idToNotice = idToNotice.updated(
            notice.id,
            noticePlace.copy(notice = Some(notice)))))
    }

  def addExpectation(noticeId: NoticeId, orderId: OrderId): Checked[BoardState] =
    idToNotice.get(noticeId) match {
      case None =>
        Right(copy(
          idToNotice = idToNotice.updated(
            noticeId,
            NoticePlace(expectation = Some(NoticeExpectation(noticeId, Set(orderId)))))))

      case Some(noticePlace) =>
        Right(copy(
          idToNotice = idToNotice.updated(
            noticeId,
            noticePlace.copy(
              expectation = {
                val exp = noticePlace.expectation getOrElse NoticeExpectation(noticeId, Set.empty)
                Some(exp.copy(orderIds = exp.orderIds + orderId))
              }))))
    }

  def removeExpectation(noticeId: NoticeId, orderId: OrderId): Checked[BoardState] =
    Right(
      idToNotice.get(noticeId) match {
        case Some(noticePlace @ NoticePlace(_, Some(expectation))) =>
          val remainingOrderIds = expectation.orderIds - orderId
          if (remainingOrderIds.isEmpty && noticePlace.notice.isEmpty)
            copy(
              idToNotice = idToNotice - noticeId)
          else
            copy(
              idToNotice = idToNotice.updated(
                noticeId,
                noticePlace.copy(
                  expectation = remainingOrderIds.nonEmpty ? expectation.copy(
                    orderIds = remainingOrderIds))))

        case _ =>
          this
      })

  def containsNotice(noticeId: NoticeId): Boolean =
    idToNotice.get(noticeId).exists(_.notice.isDefined)

  def expectingOrders(noticeId: NoticeId): Set[OrderId] =
    idToNotice.get(noticeId) match {
      case Some(NoticePlace(_, Some(NoticeExpectation(_, orderIds)))) => orderIds
      case _ => Set.empty
    }

  def notices: View[Notice] =
    idToNotice.values.view.collect { case NoticePlace(Some(o: Notice), _) => o }

  def noticeCount: Int =
    idToNotice.values.view.count {
      case NoticePlace(Some(_: Notice), _) => true
      case _ => false
    }

  def deleteNoticeEvent(noticeId: NoticeId): Checked[KeyedEvent[NoticeDeleted]] =
    for (_ <- checkDelete(noticeId)) yield
      board.path <-: NoticeDeleted(noticeId)

  def removeNotice(noticeId: NoticeId): Checked[BoardState] =
    for (_ <- checkDelete(noticeId)) yield {
      idToNotice.get(noticeId) match {
        case None | Some(NoticePlace(None, _)) =>
          this

        case Some(NoticePlace(Some(_), None)) =>
          copy(idToNotice = idToNotice - noticeId)

        case Some(noticePlace @ NoticePlace(Some(_), Some(_))) =>
          copy(idToNotice = idToNotice.updated(noticeId, noticePlace.copy(notice = None)))
      }
    }

  private def checkDelete(noticeId: NoticeId): Checked[Unit] =
    for (_ <- notice(noticeId)) yield ()

  def notice(noticeId: NoticeId): Checked[Notice] =
    for {
      noticePlace <- idToNotice.checked(noticeId)
      notice <- noticePlace match {
        case NoticePlace(Some(notice), _) =>
          Right(notice)
        case NoticePlace(None, _/*Some expected*/) =>
          Left(Problem(s"$noticeId does not denote a Notice (but a NoticeExpectation)"))
      }
    } yield notice
}
