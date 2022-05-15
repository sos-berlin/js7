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
  "BoardState snapshot" in {
    (for (snapshot <- boardState.toSnapshotObservable.toListL) yield {
      assert(snapshot == List(board, notice))

      // Order of addExpectation is irrelevant
      var recovered = BoardState(board)
      recovered = recovered.addNotice(notice).orThrow
      recovered = recovered.addExpectation(aOrderId, NoticeId("B")).orThrow
      recovered = recovered.addExpectation(bOrderId, NoticeId("B")).orThrow
      assert(recovered == boardState)

      // Now the other way round
      recovered = BoardState(board)
      recovered = recovered.addExpectation(bOrderId, NoticeId("B")).orThrow
      recovered = recovered.addExpectation(aOrderId, NoticeId("B")).orThrow
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

  private val notice = Notice(NoticeId("A"), board.path, Timestamp.ofEpochSecond(1))
  private val aOrderId = OrderId("A")
  private val bOrderId = OrderId("B")

  private val boardState = BoardState(
    board,
    View(
      notice,
      NoticeExpectation(NoticeId("B"), Set(aOrderId, bOrderId))
    ).toKeyedMap(_.id))
}
