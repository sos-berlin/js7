package js7.data.board

import js7.base.time.Timestamp
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.board.BoardStateTest._
import js7.data.order.OrderId
import js7.data.value.expression.ExpressionParser.expr
import monix.execution.Scheduler.Implicits.traced
import org.scalatest.freespec.AsyncFreeSpec
import scala.collection.View

final class BoardStateTest extends AsyncFreeSpec
{
  "addNoticeV2_3 (1)" in {
    var boardState = BoardState(board)
    val aNotice = NoticeV2_3(NoticeId("A"), endOfLife)

    boardState = boardState.addNoticeV2_3(aNotice).orThrow
    assert(boardState.idToNotice == Map(
      aNotice.id -> NoticePlace(Some(aNotice.toNotice(board.path)))))

    val a1Notice = NoticeV2_3(aNotice.id, endOfLife)
    boardState = boardState.addNoticeV2_3(a1Notice).orThrow
    assert(boardState.idToNotice == Map(
      aNotice.id -> NoticePlace(Some(a1Notice.toNotice(board.path)))))

    val bNotice = NoticeV2_3(NoticeId("B"), endOfLife)
    boardState = boardState.addNoticeV2_3(bNotice).orThrow
    assert(boardState.idToNotice == Map(
      aNotice.id -> NoticePlace(Some(aNotice.toNotice(board.path))),
      bNotice.id -> NoticePlace(Some(bNotice.toNotice(board.path)))))
  }

  "addNoticeV2_3 (2)" in {
    var boardState = BoardState(board)
    val aNotice = NoticeV2_3(NoticeId("A"), endOfLife)

    boardState = boardState.addExpectation(aNotice.id, aOrderId).orThrow
    boardState = boardState.addNoticeV2_3(aNotice).orThrow
    assert(boardState.idToNotice == Map(
      aNotice.id -> NoticePlace(
        Some(aNotice.toNotice(board.path)),
        Some(NoticeExpectation(aNotice.id, Set(aOrderId))))))
  }

  "addNotice, removeNotice (1)" in {
    var boardState = BoardState(board)
    assert(!boardState.containsNotice(aNotice.id))

    boardState = boardState.addNotice(aNotice).orThrow
    assert(boardState.idToNotice == Map(
      aNotice.id -> NoticePlace(Some(aNotice))))

    val a1Notice = Notice(aNotice.id, board.path, endOfLife)
    boardState = boardState.addNotice(a1Notice).orThrow
    assert(boardState.idToNotice == Map(
      aNotice.id -> NoticePlace(Some(a1Notice))))

    boardState = boardState.addNotice(bNotice).orThrow
    assert(boardState.idToNotice == Map(
      aNotice.id -> NoticePlace(Some(aNotice)),
      bNotice.id -> NoticePlace(Some(bNotice))))

    assert(boardState.containsNotice(aNotice.id))
    assert(boardState.containsNotice(bNotice.id))
    assert(boardState.expectingOrders(aNotice.id).isEmpty)

    boardState = boardState.removeNotice(aNotice.id).orThrow
    assert(boardState.idToNotice == Map(
      bNotice.id -> NoticePlace(Some(bNotice))))

    boardState = boardState.removeNotice(bNotice.id).orThrow
    assert(boardState.idToNotice == Map.empty)
  }

  "addNotice, removeNotice (2)" in {
    var boardState = BoardState(board)
    boardState = boardState.addExpectation(aNotice.id, aOrderId).orThrow
    boardState = boardState.addNotice(aNotice).orThrow
    assert(boardState.idToNotice == Map(
      aNotice.id -> NoticePlace(
        Some(aNotice),
        Some(NoticeExpectation(aNotice.id, Set(aOrderId))))))

    boardState = boardState.removeNotice(aNotice.id).orThrow
    assert(boardState.idToNotice == Map(
      aNotice.id -> NoticePlace(
        None,
        Some(NoticeExpectation(aNotice.id, Set(aOrderId))))))
  }

  "addExpectation, removeExpectation (1)" in {
    var boardState = BoardState(board)
    boardState = boardState.addExpectation(aNotice.id, aOrderId).orThrow
    assert(boardState.idToNotice == Map(
      aNotice.id -> NoticePlace(
        None,
        Some(NoticeExpectation(aNotice.id, Set(aOrderId))))))

    assert(boardState.expectingOrders(aNotice.id) == Set(aOrderId))
    assert(!boardState.containsNotice(aNotice.id))

    boardState = boardState.addExpectation(aNotice.id, bOrderId).orThrow
    assert(boardState.idToNotice == Map(
      aNotice.id -> NoticePlace(
        None,
        Some(NoticeExpectation(aNotice.id, Set(aOrderId, bOrderId))))))

    assert(boardState.expectingOrders(aNotice.id) == Set(aOrderId, bOrderId))
    assert(!boardState.containsNotice(aNotice.id))
    assert(boardState.notice(aNotice.id).isLeft)
    assert(boardState.notices.isEmpty)

    boardState = boardState.addNotice(aNotice).orThrow
    assert(boardState.idToNotice == Map(
      aNotice.id -> NoticePlace(
        Some(aNotice),
        Some(NoticeExpectation(aNotice.id, Set(aOrderId, bOrderId))))))
    assert(boardState.notice(aNotice.id) == Right(aNotice))
    assert(boardState.notices.toSeq == Seq(aNotice))
    assert(boardState.noticeCount == 1)

    boardState = boardState.removeExpectation(aNotice.id, aOrderId).orThrow
    assert(boardState.idToNotice == Map(
      aNotice.id -> NoticePlace(
        Some(aNotice),
        Some(NoticeExpectation(aNotice.id, Set(bOrderId))))))

    boardState = boardState.removeNotice(aNotice.id).orThrow
    assert(boardState.idToNotice == Map(
      aNotice.id -> NoticePlace(
        None,
        Some(NoticeExpectation(aNotice.id, Set(bOrderId))))))

    boardState = boardState.removeExpectation(aNotice.id, bOrderId).orThrow
    assert(boardState.idToNotice == Map.empty)
  }

  "addExpectation, removeExpectation (2)" in {
    var boardState = BoardState(board)
    boardState = boardState.addNotice(aNotice).orThrow
    boardState = boardState.addExpectation(aNotice.id, aOrderId).orThrow
    assert(boardState.idToNotice == Map(
      aNotice.id -> NoticePlace(
        Some(aNotice),
        Some(NoticeExpectation(aNotice.id, Set(aOrderId))))))

    boardState = boardState.removeExpectation(aNotice.id, aOrderId).orThrow
    assert(boardState.idToNotice == Map(
      aNotice.id -> NoticePlace(Some(aNotice))))
  }

  "BoardState snapshot" in {
    (for (snapshot <- boardState.toSnapshotObservable.toListL) yield {
      assert(snapshot == List(board, notice))

      // Order of addExpectation is irrelevant
      var recovered = BoardState(board)
      recovered = recovered.addNotice(notice).orThrow
      recovered = recovered.addExpectation(NoticeId("B"), aOrderId).orThrow
      recovered = recovered.addExpectation(NoticeId("B"), bOrderId).orThrow
      assert(recovered == boardState)

      // Now the other way round
      recovered = BoardState(board)
      recovered = recovered.addExpectation(NoticeId("B"), bOrderId).orThrow
      recovered = recovered.addExpectation(NoticeId("B"), aOrderId).orThrow
      recovered = recovered.addNotice(notice).orThrow
      assert(recovered == boardState)
    }).runToFuture
  }
}

private object BoardStateTest
{
  private val board = Board(
    BoardPath("BOARD"),
    postOrderToNoticeId = expr("'NOTICE'"),
    expectOrderToNoticeId = expr("'NOTICE'"),
    endOfLife = expr("$js7EpochMilli + 24 * 3600 * 1000"))

  private val endOfLife = Timestamp.ofEpochSecond(1)
  private val notice = Notice(NoticeId("A"), board.path, endOfLife)
  private val aOrderId = OrderId("A")
  private val bOrderId = OrderId("B")

  private val aNotice = Notice(NoticeId("A"), board.path, endOfLife)
  private val bNotice = Notice(NoticeId("B"), board.path, endOfLife)

  private val boardState = BoardState(
    board,
    View(
      NoticePlace(Some(notice)),
      NoticePlace(None, Some(NoticeExpectation(NoticeId("B"), Set(aOrderId, bOrderId))))
    ).toKeyedMap(_.id))
}
