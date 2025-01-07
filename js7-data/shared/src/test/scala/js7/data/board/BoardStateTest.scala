package js7.data.board

import cats.syntax.option.*
import io.circe.Printer
import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils.{JsonStringInterpolator, RichJson, parseJson, reparseJson}
import js7.base.test.OurAsyncTestSuite
import js7.base.time.Timestamp
import js7.base.utils.CatsUtils.Nel
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.board.BoardStateTest.*
import js7.data.controller.ControllerState
import js7.data.order.OrderId
import js7.data.plan.{PlanId, PlanKey, PlanSchemaId}
import js7.data.value.expression.ExpressionParser.expr
import scala.collection.View

final class BoardStateTest extends OurAsyncTestSuite:

  "JSON" - {
    val boardPath = BoardPath("BOARD")

    "v2.4 JSON compatibility" in:
      val boardState = BoardState(
        GlobalBoard(
          boardPath,
          postOrderToNoticeId =
            expr("""replaceAll($js7OrderId, '^#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*$', '$1')"""),
          expectOrderToNoticeId =
            expr("""replaceAll($js7OrderId, '^#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*$', '$1')"""),
          endOfLife = expr("$js7EpochMilli + 24 * 3600 * 1000")),
        idToNotice = Map(
          NoticeId("NOTICE") -> NoticePlace(
            NoticeId("NOTICE"),
            Some(Notice(NoticeId("NOTICE"), boardPath,
              endOfLife = Timestamp.ofEpochSecond(123).some)))))

      boardState.toSnapshotStream
        .map(_
          .asJson(ControllerState.snapshotObjectJsonCodec)
          .compactPrint)
        .map(s => parseJson(s).orThrow)
        .compile
        .toVector
        .map: snapshots =>
          assert(snapshots == List(
            json"""{
              "TYPE": "GlobalBoard",
              "path": "BOARD",
              "postOrderToNoticeId":
                "replaceAll($$js7OrderId, '^#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*$$', '$$1')",
              "expectOrderToNoticeId":
                "replaceAll($$js7OrderId, '^#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*$$', '$$1')",
              "endOfLife": "$$js7EpochMilli + 24 * 3600 * 1000"
            }""",
            json"""{
              "TYPE": "Notice",
              "id": "NOTICE",
              "boardPath": "BOARD",
              "endOfLife": 123000
            }"""))


    "for GlobalBoard" - {
      lazy val boardState = BoardState(
        GlobalBoard(
          boardPath,
          postOrderToNoticeId =
            expr("""match(orderId, '#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*', '$1')?"""),
          expectOrderToNoticeId =
            expr("""match(orderId, '#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*', '$1')?"""),
          endOfLife = expr("$js7EpochMilli + 24 * 3600 * 1000")),
        idToNotice = Map(
          NoticeId("NOTICE-1") -> NoticePlace(
            NoticeId("NOTICE-1"),
            isInConsumption = true),
          NoticeId("NOTICE-2") -> NoticePlace(
            NoticeId("NOTICE-2"),
            Some(Notice(NoticeId("NOTICE-2"), boardPath,
              endOfLife = Timestamp.ofEpochSecond(123).some)),
            expectingOrderIds = Set.empty /*Recovered by Order.ExpectingNotices*/ ,
            consumptionCount = 7)),
        orderToConsumptionStack = Map(
          OrderId("A-ORDER") -> Nel.of(
            NoticeId("NOTICE-3"),
            NoticeId("NOTICE-2"),
            NoticeId("NOTICE-1"))))

      "toSnapshotStream JSON" in:
        boardState.toSnapshotStream
          .map(_
            .asJson(ControllerState.snapshotObjectJsonCodec)
            .printWith(Printer.noSpaces.copy(dropNullValues = true)))
          .map(s => io.circe.parser.parse(s).orThrow)
          .compile.toVector
          .map(snapshots =>
            assert(snapshots == List(
              json"""{
                "TYPE": "GlobalBoard",
                "path": "BOARD",
                "postOrderToNoticeId":
                  "match(orderId, '#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*', '$$1')?",
                "expectOrderToNoticeId":
                  "match(orderId, '#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*', '$$1')?",
                "endOfLife": "$$js7EpochMilli + 24 * 3600 * 1000"
              }""", json"""{
                "TYPE": "Notice",
                "id": "NOTICE-2",
                "boardPath": "BOARD",
                "endOfLife": 123000
              }""", json"""{
                "TYPE": "NoticePlace",
                "boardPath": "BOARD",
                "noticeId": "NOTICE-1",
                "isAnnounced": false,
                "isInConsumption": true,
                "consumptionCount": 0
              }""", json"""{
                "TYPE": "NoticePlace",
                "boardPath": "BOARD",
                "noticeId": "NOTICE-2",
                "isAnnounced": false,
                "isInConsumption": false,
                "consumptionCount": 7
              }""", json"""{
                "TYPE": "NoticeConsumption",
                "orderId": "A-ORDER",
                "boardPath": "BOARD",
                "noticeIdStack": [ "NOTICE-3", "NOTICE-2", "NOTICE-1" ]
              }""")))

      "toSnapshotStream and recover" in:
        import scala.language.unsafeNulls

        var recovered: BoardState = null
        boardState.toSnapshotStream
          .map(o =>
            reparseJson(o, ControllerState.snapshotObjectJsonCodec).orThrow)
          .map:
            case board: GlobalBoard =>
              recovered = BoardState(board)
            case snapshot: NoticeSnapshot =>
              recovered = recovered.recover(snapshot).orThrow
          .compile
          .drain
          .map: _ =>
            assert(recovered == boardState)
    }

    "for PlannableBoard" - {
      val planId = PlanId(PlanSchemaId("DailyPlan"), PlanKey("2024-11-08"))
      lazy val boardState = BoardState(
        PlannableBoard(boardPath),
        idToNotice = Map(
          planId.noticeId -> NoticePlace(
            planId.noticeId,
            isInConsumption = true),
          NoticeId(NoticeKey("NOTICE-2"), planId) -> NoticePlace(
            NoticeId(NoticeKey("NOTICE-2"), planId),
            Some(Notice(NoticeId(NoticeKey("NOTICE-2"), planId), boardPath,
              endOfLife = Timestamp.ofEpochSecond(123).some)),
            expectingOrderIds = Set.empty /*Recovered by Order.ExpectingNotices*/ ,
            consumptionCount = 7)),
        orderToConsumptionStack = Map(
          OrderId("A-ORDER") -> Nel.of(
            planId.noticeId,
            NoticeId(NoticeKey("NOTICE-2"), planId),
            planId.noticeId)))

      "toSnapshotStream JSON" in:
        boardState.toSnapshotStream
          .map(_
            .asJson(ControllerState.snapshotObjectJsonCodec)
            .printWith(Printer.noSpaces.copy(dropNullValues = true)))
          .map(s => io.circe.parser.parse(s).orThrow)
          .compile.toVector
          .map(snapshots =>
            assert(snapshots == List(
              json"""{
                "TYPE": "PlannableBoard",
                "path": "BOARD"
              }""", json"""{
                "TYPE": "Notice",
                "id": [ "DailyPlan", "2024-11-08", "NOTICE-2" ],
                "boardPath": "BOARD",
                "endOfLife": 123000
              }""", json"""{
                "TYPE": "NoticePlace",
                "boardPath": "BOARD",
                "noticeId": [ "DailyPlan", "2024-11-08" ],
                "isAnnounced": false,
                "isInConsumption": true,
                "consumptionCount": 0
              }""", json"""{
                "TYPE": "NoticePlace",
                "boardPath": "BOARD",
                "noticeId": [ "DailyPlan", "2024-11-08", "NOTICE-2" ],
                "isAnnounced": false,
                "isInConsumption": false,
                "consumptionCount": 7
              }""", json"""{
                "TYPE": "NoticeConsumption",
                "orderId": "A-ORDER",
                "boardPath": "BOARD",
                "noticeIdStack": [
                  [ "DailyPlan", "2024-11-08" ],
                  [ "DailyPlan", "2024-11-08", "NOTICE-2" ],
                  [ "DailyPlan", "2024-11-08" ]
                ]
              }""")))

      "toSnapshotStream and recover" in:
        import scala.language.unsafeNulls

        var recovered: BoardState = null
        boardState.toSnapshotStream
          .map(o =>
            reparseJson(o, ControllerState.snapshotObjectJsonCodec).orThrow)
          .map:
            case board: PlannableBoard =>
              recovered = BoardState(board)
            case snapshot: NoticeSnapshot =>
              recovered = recovered.recover(snapshot).orThrow
          .compile
          .drain
          .map: _ =>
            assert(recovered == boardState)
    }
  }

  "addNoticeV2_3 (1)" in:
    var boardState = BoardState(board)
    val aNotice = NoticeV2_3(NoticeId("A"), endOfLife)

    boardState = boardState.addNoticeV2_3(aNotice).orThrow
    assert(boardState.idToNotice == Map(
      aNotice.id -> NoticePlace(aNotice.id, Some(aNotice.toNotice(board.path)))))

    val a1Notice = NoticeV2_3(aNotice.id, endOfLife)
    boardState = boardState.addNoticeV2_3(a1Notice).orThrow
    assert(boardState.idToNotice == Map(
      aNotice.id -> NoticePlace(a1Notice.id, Some(a1Notice.toNotice(board.path)))))

    val bNotice = NoticeV2_3(NoticeId("B"), endOfLife)
    boardState = boardState.addNoticeV2_3(bNotice).orThrow
    assert(boardState.idToNotice == Map(
      aNotice.id -> NoticePlace(aNotice.id, Some(aNotice.toNotice(board.path))),
      bNotice.id -> NoticePlace(bNotice.id, Some(bNotice.toNotice(board.path)))))

  "addNoticeV2_3 (2)" in:
    var boardState = BoardState(board)
    val aNotice = NoticeV2_3(NoticeId("A"), endOfLife)

    boardState = boardState.addExpectation(aNotice.id, aOrderId).orThrow
    boardState = boardState.addNoticeV2_3(aNotice).orThrow
    assert(boardState.idToNotice == Map(
      aNotice.id -> NoticePlace(
        aNotice.id,
        Some(aNotice.toNotice(board.path)),
        Set(aOrderId))))

  "addNotice, removeNotice (1)" in:
    var boardState = BoardState(board)
    assert(!boardState.containsNotice(aNotice.id))

    boardState = boardState.addNotice(aNotice).orThrow
    assert(boardState.idToNotice == Map(
      aNotice.id -> NoticePlace(aNotice.id, Some(aNotice))))

    val a1Notice = Notice(aNotice.id, board.path, endOfLife.some)
    boardState = boardState.addNotice(a1Notice).orThrow
    assert(boardState.idToNotice == Map(
      aNotice.id -> NoticePlace(a1Notice.id, Some(a1Notice))))

    boardState = boardState.addNotice(bNotice).orThrow
    assert(boardState.idToNotice == Map(
      aNotice.id -> NoticePlace(aNotice.id, Some(aNotice)),
      bNotice.id -> NoticePlace(bNotice.id, Some(bNotice))))

    assert(boardState.containsNotice(aNotice.id))
    assert(boardState.containsNotice(bNotice.id))
    assert(boardState.expectingOrders(aNotice.id).isEmpty)

    boardState = boardState.removeNotice(aNotice.id).orThrow
    assert(boardState.idToNotice == Map(
      bNotice.id -> NoticePlace(bNotice.id, Some(bNotice))))

    boardState = boardState.removeNotice(bNotice.id).orThrow
    assert(boardState.idToNotice == Map.empty)

  "addNotice, removeNotice (2)" in:
    var boardState = BoardState(board)
    boardState = boardState.addExpectation(aNotice.id, aOrderId).orThrow
    boardState = boardState.addNotice(aNotice).orThrow
    assert(boardState.idToNotice == Map(
      aNotice.id -> NoticePlace(
        aNotice.id,
        Some(aNotice),
        Set(aOrderId))))

    boardState = boardState.removeNotice(aNotice.id).orThrow
    assert(boardState.idToNotice == Map(
      aNotice.id -> NoticePlace(
        aNotice.id,
        None,
        Set(aOrderId))))

  "addExpectation, removeExpectation (1)" in:
    var boardState = BoardState(board)
    boardState = boardState.addExpectation(aNotice.id, aOrderId).orThrow
    assert(boardState.idToNotice == Map(
      aNotice.id -> NoticePlace(
        aNotice.id,
        None,
        Set(aOrderId))))

    assert(boardState.expectingOrders(aNotice.id) == Set(aOrderId))
    assert(!boardState.containsNotice(aNotice.id))

    boardState = boardState.addExpectation(aNotice.id, bOrderId).orThrow
    assert(boardState.idToNotice == Map(
      aNotice.id -> NoticePlace(
        aNotice.id,
        None,
        Set(aOrderId, bOrderId))))

    assert(boardState.expectingOrders(aNotice.id) == Set(aOrderId, bOrderId))
    assert(!boardState.containsNotice(aNotice.id))
    assert(boardState.notice(aNotice.id).isLeft)
    assert(boardState.notices.isEmpty)

    boardState = boardState.addNotice(aNotice).orThrow
    assert(boardState.idToNotice == Map(
      aNotice.id -> NoticePlace(
        aNotice.id,
        Some(aNotice),
        Set(aOrderId, bOrderId))))
    assert(boardState.notice(aNotice.id) == Right(aNotice))
    assert(boardState.notices.toSeq == Seq(aNotice))

    boardState = boardState.removeExpectation(aNotice.id, aOrderId).orThrow
    assert(boardState.idToNotice == Map(
      aNotice.id -> NoticePlace(
        aNotice.id,
        Some(aNotice),
        Set(bOrderId))))

    boardState = boardState.removeNotice(aNotice.id).orThrow
    assert(boardState.idToNotice == Map(
      aNotice.id -> NoticePlace(
        aNotice.id,
        None,
        Set(bOrderId))))

    boardState = boardState.removeExpectation(aNotice.id, bOrderId).orThrow
    assert(boardState.idToNotice == Map.empty)

  "addExpectation, removeExpectation (2)" in:
    var boardState = BoardState(board)
    boardState = boardState.addNotice(aNotice).orThrow
    boardState = boardState.addExpectation(aNotice.id, aOrderId).orThrow
    assert(boardState.idToNotice == Map(
      aNotice.id -> NoticePlace(
        aNotice.id,
        Some(aNotice),
        Set(aOrderId))))

    boardState = boardState.removeExpectation(aNotice.id, aOrderId).orThrow
    assert(boardState.idToNotice == Map(
      aNotice.id -> NoticePlace(aNotice.id, Some(aNotice))))

  "BoardState snapshot" in:
    for snapshot <- boardState.toSnapshotStream.compile.toVector yield
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

private object BoardStateTest:

  private val board = GlobalBoard(
    BoardPath("BOARD"),
    postOrderToNoticeId = expr("'NOTICE'"),
    expectOrderToNoticeId = expr("'NOTICE'"),
    endOfLife = expr("$js7EpochMilli + 24 * 3600 * 1000"))

  private val endOfLife = Timestamp.ofEpochSecond(1)
  private val notice = Notice(NoticeId("A"), board.path, endOfLife.some)
  private val aOrderId = OrderId("A")
  private val bOrderId = OrderId("B")

  private val aNotice = Notice(NoticeId("A"), board.path, endOfLife.some)
  private val bNotice = Notice(NoticeId("B"), board.path, endOfLife.some)

  private val boardState = BoardState(
    board,
    View(
      NoticePlace(notice.id, Some(notice)),
      NoticePlace(NoticeId("B"), None, Set(aOrderId, bOrderId))
    ).toKeyedMap(_.noticeId))
