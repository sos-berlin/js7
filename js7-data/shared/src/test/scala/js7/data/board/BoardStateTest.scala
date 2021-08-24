package js7.data.board

import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.board.BoardStateTest._
import js7.data.order.OrderId
import js7.data.value.expression.ExpressionParser.expr
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.View

final class BoardStateTest extends AnyFreeSpec
{
  "BoardState snapshot" in {
    val snapshot = boardState.toSnapshotObservable.toListL.await(99.s)
    assert(snapshot == List(
      board,
      Notice.Snapshot(board.path, notice)))

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
  }
}

private object BoardStateTest
{
  private val board = Board(
    BoardPath("BOARD"),
    postOrderToNoticeId = expr("'NOTICE'"),
    expectOrderToNoticeId = expr("'NOTICE'"),
    endOfLife = expr("$epochMillis + 24 * 3600 * 1000"))

  private val notice = Notice(NoticeId("A"), Timestamp.ofEpochSecond(1))
  private val aOrderId = OrderId("A")
  private val bOrderId = OrderId("B")

  private val boardState = BoardState(
    board,
    View(
      notice,
      NoticeExpectation(NoticeId("B"), Set(aOrderId, bOrderId))
    ).toKeyedMap(_.id))
}
