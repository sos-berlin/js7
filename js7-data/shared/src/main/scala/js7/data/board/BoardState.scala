package js7.data.board

import cats.syntax.traverse.*
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.Subtype
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.board.BoardEvent.NoticeDeleted
import js7.data.board.BoardState.Consumption
import js7.data.event.KeyedEvent
import js7.data.item.UnsignedSimpleItemState
import js7.data.order.OrderEvent.OrderNoticesConsumptionStarted
import js7.data.order.{Order, OrderId}
import monix.reactive.Observable
import scala.collection.View

final case class BoardState(
  board: Board,
  idToNotice: Map[NoticeId, NoticePlace] = Map.empty,
  orderToConsumptions: Map[OrderId, List[Consumption]] = Map.empty)
extends UnsignedSimpleItemState
{
  protected type Self = BoardState
  val companion: BoardState.type = BoardState

  val item: Board = board
  def path: BoardPath = item.path

  def updateItem(item: Board): Checked[BoardState] =
    Right(copy(board = item))

  override def toString = s"BoardState(${board.pathRev} $idToNotice)"

  override def toSnapshotObservable: Observable[Any/*BoardState | BoardSnapshot*/] =
    // Notice expectations are recovered from Order[Order.ExpectingNotice]
    Observable.fromIterable(
      (board +: notices.view) ++
        idToNotice.values.view.flatMap(_.toSnapshot(path)) ++
        orderToConsumptions.view
          .flatMap { case (orderId, consumptions) =>
            consumptions.reverse.map(Consumption.Snapshot(path, orderId, _))
          })

  def recover(snapshot: BoardSnapshot): Checked[BoardState] =
    snapshot match {
      case notice: Notice =>
        addNotice(notice)

      case snapshot: BoardState.Consumption.Snapshot =>
        Right(copy(
          orderToConsumptions = orderToConsumptions.updated(snapshot.orderId,
            snapshot.consumption :: orderToConsumptions.getOrElse(snapshot.orderId, Nil))))

      case snapshot: NoticePlace.Snapshot =>
        Right(copy(
          idToNotice = idToNotice.updated(snapshot.noticeId,
            idToNotice.getOrElse(snapshot.noticeId, NoticePlace(snapshot.noticeId))
              .withSnapshot(snapshot))))
    }

  // COMPATIBLE with v2.3
  def addNoticeV2_3(notice: NoticeV2_3): Checked[BoardState] =
    addNotice(notice.toNotice(board.path))

  def addNotice(notice: Notice): Checked[BoardState] =
    idToNotice.get(notice.id) match {
      case None =>
        Right(updateNoticePlace(NoticePlace(notice.id, Some(notice))))

      case Some(noticePlace) =>
        Right(updateNoticePlace(noticePlace.post(notice)))
    }

  def addExpectation(noticeId: NoticeId, orderId: OrderId): Checked[BoardState] = {
    val noticePlace = idToNotice.getOrElse(noticeId, NoticePlace(noticeId))
    Right(updateNoticePlace(noticePlace.copy(
      expectingOrderIds = noticePlace.expectingOrderIds + orderId)))
  }

  def removeExpectation(noticeId: NoticeId, orderId: OrderId): Checked[BoardState] =
    Right(
      idToNotice.get(noticeId).fold(this)(noticePlace =>
        updateNoticePlace(noticePlace.copy(
          expectingOrderIds = noticePlace.expectingOrderIds - orderId))))

  def addConsumption(
    noticeId: NoticeId,
    order: Order[Order.State],
    consumingSeq: Vector[OrderNoticesConsumptionStarted.Consuming])
  : Checked[BoardState] = {
    val noticePlace = idToNotice.getOrElse(noticeId, NoticePlace(noticeId))
      .startConsuming
    val consumptions = orderToConsumptions.getOrElse(order.id, Nil)
    val consumption = Consumption(/*order.workflowPosition, */consumingSeq)
    Right(copy(
      idToNotice = idToNotice.updated(noticeId,
        noticePlace.copy(
          expectingOrderIds = noticePlace.expectingOrderIds - order.id)),
      orderToConsumptions = orderToConsumptions.updated(order.id, consumption :: consumptions)))
  }

  def removeConsumption(orderId: OrderId, succeeded: Boolean): Checked[BoardState] =
    orderToConsumptions.checked(orderId)
      .flatMap { consumptions =>
        val head = consumptions.head
        val tail = consumptions.tail
        head.consuming
          .map(_.noticeId)
          .traverse(idToNotice.checked)
          .map(_
            .foldLeft(this)((boardState, noticePlace) =>
              boardState.updateNoticePlace(
                noticePlace.finishConsuming(succeeded)))
            .copy(
              orderToConsumptions =
                if (tail.isEmpty)
                  orderToConsumptions - orderId
                else
                  orderToConsumptions.updated(orderId, tail)))
      }

  def containsNotice(noticeId: NoticeId): Boolean =
    idToNotice.get(noticeId).exists(_.notice.isDefined)

  def expectingOrders(noticeId: NoticeId): Set[OrderId] =
    idToNotice.get(noticeId).fold_(Set.empty, _.expectingOrderIds)

  def notices: View[Notice] =
    idToNotice.values.view.flatMap(_.notice)

  def noticeCount: Int =
    idToNotice.values.count(_.notice.isDefined)

  def deleteNoticeEvent(noticeId: NoticeId): Checked[KeyedEvent[NoticeDeleted]] =
    for (_ <- checkDelete(noticeId)) yield
      board.path <-: NoticeDeleted(noticeId)

  def removeNotice(noticeId: NoticeId): Checked[BoardState] =
    for (_ <- checkDelete(noticeId)) yield {
      idToNotice.get(noticeId) match {
        case None =>
          this

        case Some(noticePlace) =>
          updateNoticePlace(noticePlace.removeNotice)
      }
    }

  private def updateNoticePlace(noticePlace: NoticePlace): BoardState =
    copy(
      idToNotice =
        if (noticePlace.isEmpty)
          idToNotice - noticePlace.noticeId
        else
          idToNotice.updated(noticePlace.noticeId, noticePlace))

  private def checkDelete(noticeId: NoticeId): Checked[Unit] =
    for (_ <- notice(noticeId)) yield ()

  def notice(noticeId: NoticeId): Checked[Notice] =
    for {
      noticePlace <- idToNotice.checked(noticeId)
      notice <- noticePlace.notice match {
        case None => Left(Problem(s"$noticeId does not denote a Notice (but a Notice expectation)"))
        case Some(notice) => Right(notice)
      }
    } yield notice
}

object BoardState extends UnsignedSimpleItemState.Companion[BoardState] {
  type Key = BoardPath
  type Item = Board
  override type ItemState = BoardState

  final case class Consumption(
    //workflowPosition: WorkflowPosition,
    consuming: Vector[OrderNoticesConsumptionStarted.Consuming])
  object Consumption
  {
    implicit val jsonCodec: Codec.AsObject[Consumption] = deriveCodec

    final case class Snapshot(boardPath: BoardPath, orderId: OrderId, consumption: Consumption)
    extends BoardSnapshot
    object Snapshot {
      val subtype = Subtype.named(deriveCodec[Snapshot], "NoticeConsumption")
    }
  }
}
