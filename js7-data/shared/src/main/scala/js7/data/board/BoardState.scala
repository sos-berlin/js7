package js7.data.board

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import js7.base.problem.{Checked, Problem}
import js7.base.utils.typeclasses.IsEmpty.syntax.toIsEmptyAllOps
import js7.data.order.OrderId

final case class BoardState(
  board: Board,
  idToNotice: Map[NoticeId, NoticeIdState] = Map.empty)
{
  def path = board.path

  def addNotice(notice: Notice): BoardState =
    copy(
      idToNotice = idToNotice + (notice.id -> notice))

  def addWaitingOrder(orderId: OrderId, noticeId: NoticeId): Checked[BoardState] =
    idToNotice.get(noticeId) match {
      case None =>
        Right(copy(
          idToNotice = idToNotice +
            (noticeId -> AwaitingNotice(noticeId, orderId :: Nil))))

      case Some(awaitingNotice: AwaitingNotice) =>
        Right(copy(
          idToNotice = idToNotice +
            (noticeId -> awaitingNotice.copy(
              awaitingOrderIds = awaitingNotice.awaitingOrderIds.view.appended(orderId).toVector))))

      case Some(_: Notice) =>
        Left(Problem("BoardState.addWaitingOrder but notice has been posted"))
    }

  def waitingOrders(noticeId: NoticeId): Seq[OrderId] =
    idToNotice.get(noticeId) match {
      case Some(AwaitingNotice(_, orderIds)) => orderIds
      case _ => Nil
    }

  def notices: Iterable[Notice] =
    idToNotice.values.view.collect { case o: Notice => o }

  def deleteNotice(noticeId: NoticeId): BoardState =
    copy(idToNotice = idToNotice - noticeId)
}

object BoardState
{
  implicit val jsonEncoder: Encoder.AsObject[BoardState] =  // TODO Big
    o => JsonObject(
      "board" -> o.board.asJson,
      "notices" -> o.idToNotice.values.??.asJson)

  implicit val jsonDecoder: Decoder[BoardState] =
    c => for {
      board <- c.get[Board]("board")
      notices <- c.getOrElse[Vector[NoticeIdState]]("notices")(Vector.empty)
    } yield BoardState(board, notices.view.map(o => o.id -> o).toMap)
}
