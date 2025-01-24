package js7.data.board

import cats.syntax.option.*
import io.circe.syntax.EncoderOps
import io.circe.{Codec, Printer}
import js7.base.circeutils.CirceUtils.{JsonStringInterpolator, RichJson, parseJson, reparseJson}
import js7.base.circeutils.typed.TypedJsonCodec
import js7.base.test.OurAsyncTestSuite
import js7.base.time.Timestamp
import js7.base.utils.CatsUtils.Nel
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.board.BoardState.NoticeConsumptionSnapshot
import js7.data.board.BoardStateTest.*
import js7.data.controller.ControllerState
import js7.data.order.OrderId
import js7.data.plan.{PlanId, PlanKey, PlanSchemaId}
import js7.data.value.expression.ExpressionParser.expr
import js7.tester.CirceJsonTester
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}
import scala.collection.View

final class BoardStateTest extends OurAsyncTestSuite:

  "JSON" - {
    val boardPath = BoardPath("BOARD")

    "NoticeConsumptionSnapshot" in:
      given Codec.AsObject[NoticeConsumptionSnapshot] = TypedJsonCodec(NoticeConsumptionSnapshot.subtype)

      testJson(
        NoticeConsumptionSnapshot(
          boardPath,
          OrderId("ORDER"),
          Nel.of(
            PlanSchemaId("DailyPlan") / "2025-01-17" / NoticeKey("NOTICE"),
            PlanSchemaId("DailyPlan") / "2025-01-17" / NoticeKey.empty,
            PlanId.Global / NoticeKey("NOTICE"))),
        json"""{
          "TYPE": "NoticeConsumption",
          "orderId": "ORDER",
          "boardPath": "BOARD",
          "plannedNoticeKeyStack": [
            [ "DailyPlan", "2025-01-17", "NOTICE" ],
            [ "DailyPlan", "2025-01-17" ],
            "NOTICE"
          ]
        }""")

      testJsonDecoder(
        NoticeConsumptionSnapshot(
          boardPath,
          OrderId("ORDER"),
          Nel.of:
            PlanId.Global / NoticeKey("NOTICE")),
        json"""{
          "TYPE": "NoticeConsumption",
          "orderId": "ORDER",
          "boardPath": "BOARD",
          "noticeIdStack": [
            "NOTICE"
          ]
        }""")


    "v2.4 JSON compatibility" in:
      val boardState = BoardState(
        GlobalBoard(
          boardPath,
          postOrderToNoticeKey =
            expr("""replaceAll($js7OrderId, '^#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*$', '$1')"""),
          expectOrderToNoticeKey =
            expr("""replaceAll($js7OrderId, '^#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*$', '$1')"""),
          endOfLife = expr("$js7EpochMilli + 24 * 3600 * 1000")),
        toNoticePlace = Map(
          GlobalNoticeKey("NOTICE") -> NoticePlace(
            Some(Notice(
              boardPath / GlobalNoticeKey("NOTICE"),
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
              "postOrderToNoticeKey":
                "replaceAll($$js7OrderId, '^#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*$$', '$$1')",
              "expectOrderToNoticeKey":
                "replaceAll($$js7OrderId, '^#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*$$', '$$1')",
              "endOfLife": "$$js7EpochMilli + 24 * 3600 * 1000"
            }""",
            json"""{
              "TYPE": "Notice",
              "id": [ "BOARD", "NOTICE" ],
              "endOfLife": 123000
            }"""))

    "for GlobalBoard" - {
      lazy val boardState = BoardState(
        GlobalBoard(
          boardPath,
          postOrderToNoticeKey =
            expr("""match(orderId, '#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*', '$1')?"""),
          expectOrderToNoticeKey =
            expr("""match(orderId, '#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*', '$1')?"""),
          endOfLife = expr("$js7EpochMilli + 24 * 3600 * 1000")),
        toNoticePlace = Map(
          GlobalNoticeKey("NOTICE-1") -> NoticePlace(
            isInConsumption = true),
          GlobalNoticeKey("NOTICE-2") -> NoticePlace(
            Some(Notice(
              boardPath / GlobalNoticeKey("NOTICE-2"),
              endOfLife = Timestamp.ofEpochSecond(123).some)),
            expectingOrderIds = Set.empty /*Recovered by Order.ExpectingNotices*/ ,
            consumptionCount = 7)),
        orderToConsumptionStack = Map(
          OrderId("A-ORDER") -> Nel.of(
            GlobalNoticeKey("NOTICE-3"),
            GlobalNoticeKey("NOTICE-2"),
            GlobalNoticeKey("NOTICE-1"))))

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
                "postOrderToNoticeKey":
                  "match(orderId, '#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*', '$$1')?",
                "expectOrderToNoticeKey":
                  "match(orderId, '#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*', '$$1')?",
                "endOfLife": "$$js7EpochMilli + 24 * 3600 * 1000"
              }""", json"""{
                "TYPE": "Notice",
                "id": [ "BOARD" ,"NOTICE-2" ],
                "endOfLife": 123000
              }""", json"""{
                "TYPE": "NoticePlace",
                "noticeId": [ "BOARD" ,"NOTICE-1" ],
                "isAnnounced": false,
                "isInConsumption": true,
                "consumptionCount": 0
              }""", json"""{
                "TYPE": "NoticePlace",
                "noticeId": [ "BOARD", "NOTICE-2" ],
                "isAnnounced": false,
                "isInConsumption": false,
                "consumptionCount": 7
              }""", json"""{
                "TYPE": "NoticeConsumption",
                "boardPath": "BOARD",
                "orderId": "A-ORDER",
                "plannedNoticeKeyStack": [ "NOTICE-3", "NOTICE-2", "NOTICE-1" ]
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
        toNoticePlace = Map(
          planId / NoticeKey.empty -> NoticePlace(
            isInConsumption = true),
          planId / NoticeKey("NOTICE-2") -> NoticePlace(
            Some(Notice(
              planId / boardPath / "NOTICE-2",
              endOfLife = Timestamp.ofEpochSecond(123).some)),
            expectingOrderIds = Set.empty /*Recovered by Order.ExpectingNotices*/ ,
            consumptionCount = 7)),
        orderToConsumptionStack = Map(
          OrderId("A-ORDER") -> Nel.of(
            planId / NoticeKey.empty,
            planId / NoticeKey("NOTICE-2"),
            planId / NoticeKey.empty)))

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
                "path": "BOARD",
                "postOrderToNoticeKey" : "\"\"",
                "expectOrderToNoticeKey" : "\"\""
              }""", json"""{
                "TYPE": "Notice",
                "id": [ "DailyPlan", "2024-11-08", "BOARD", "NOTICE-2" ],
                "endOfLife": 123000
              }""", json"""{
                "TYPE": "NoticePlace",
                "noticeId": [ "DailyPlan", "2024-11-08", "BOARD" ],
                "isAnnounced": false,
                "isInConsumption": true,
                "consumptionCount": 0
              }""", json"""{
                "TYPE": "NoticePlace",
                "noticeId": [ "DailyPlan", "2024-11-08", "BOARD", "NOTICE-2" ],
                "isAnnounced": false,
                "isInConsumption": false,
                "consumptionCount": 7
              }""", json"""{
                "TYPE": "NoticeConsumption",
                "orderId": "A-ORDER",
                "boardPath": "BOARD",
                "plannedNoticeKeyStack": [
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
    val aNotice = NoticeV2_3(NoticeKey("A"), endOfLife)

    boardState = boardState.addNoticeV2_3(aNotice).orThrow
    assert(boardState.toNoticePlace == Map(
      PlanId.Global / aNotice.noticeKey -> NoticePlace(Some(aNotice.toNotice(board.path)))))

    val a1Notice = NoticeV2_3(aNotice.noticeKey, endOfLife)
    boardState = boardState.addNoticeV2_3(a1Notice).orThrow
    assert(boardState.toNoticePlace == Map(
      PlanId.Global / aNotice.noticeKey -> NoticePlace(Some(a1Notice.toNotice(board.path)))))

    val bNotice = NoticeV2_3(NoticeKey("B"), endOfLife)
    boardState = boardState.addNoticeV2_3(bNotice).orThrow
    assert(boardState.toNoticePlace == Map(
      PlanId.Global / aNotice.noticeKey -> NoticePlace(Some(aNotice.toNotice(board.path))),
      PlanId.Global / bNotice.noticeKey -> NoticePlace(Some(bNotice.toNotice(board.path)))))

  "addNoticeV2_3 (2)" in:
    var boardState = BoardState(board)
    val aNotice = NoticeV2_3(NoticeKey("A"), endOfLife)

    boardState = boardState.addExpectation(GlobalNoticeKey(aNotice.noticeKey), aOrderId).orThrow
    boardState = boardState.addNoticeV2_3(aNotice).orThrow
    assert(boardState.toNoticePlace == Map(
      PlanId.Global / aNotice.noticeKey -> NoticePlace(
        Some(aNotice.toNotice(board.path)),
        Set(aOrderId))))

  "addNotice, removeNotice (1)" in:
    var boardState = BoardState(board)
    assert(!boardState.hasNotice(aNotice.plannedNoticeKey))

    boardState = boardState.addNotice(aNotice).orThrow
    assert(boardState.toNoticePlace == Map(
      aNotice.plannedNoticeKey -> NoticePlace(Some(aNotice))))

    val a1Notice = Notice(board.path / aNotice.plannedNoticeKey, endOfLife.some)
    boardState = boardState.addNotice(a1Notice).orThrow
    assert(boardState.toNoticePlace == Map(
      aNotice.plannedNoticeKey -> NoticePlace(Some(a1Notice))))

    boardState = boardState.addNotice(bNotice).orThrow
    assert(boardState.toNoticePlace == Map(
      aNotice.plannedNoticeKey -> NoticePlace(Some(aNotice)),
      bNotice.plannedNoticeKey -> NoticePlace(Some(bNotice))))

    assert(boardState.hasNotice(aNotice.plannedNoticeKey))
    assert(boardState.hasNotice(bNotice.plannedNoticeKey))
    assert(boardState.expectingOrders(aNotice.plannedNoticeKey).isEmpty)

    boardState = boardState.removeNotice(aNotice.plannedNoticeKey).orThrow
    assert(boardState.toNoticePlace == Map(
      bNotice.plannedNoticeKey -> NoticePlace(Some(bNotice))))

    boardState = boardState.removeNotice(bNotice.plannedNoticeKey).orThrow
    assert(boardState.toNoticePlace == Map.empty)

  "addNotice, removeNotice (2)" in:
    var boardState = BoardState(board)
    boardState = boardState.addExpectation(aNotice.plannedNoticeKey, aOrderId).orThrow
    boardState = boardState.addNotice(aNotice).orThrow
    assert(boardState.toNoticePlace == Map(
      aNotice.plannedNoticeKey -> NoticePlace(
        Some(aNotice),
        Set(aOrderId))))

    boardState = boardState.removeNotice(aNotice.plannedNoticeKey).orThrow
    assert(boardState.toNoticePlace == Map(
      aNotice.plannedNoticeKey -> NoticePlace(
        None,
        Set(aOrderId))))

  "addExpectation, removeExpectation (1)" in:
    var boardState = BoardState(board)
    boardState = boardState.addExpectation(aNotice.plannedNoticeKey, aOrderId).orThrow
    assert(boardState.toNoticePlace == Map(
      aNotice.plannedNoticeKey -> NoticePlace(
        None,
        Set(aOrderId))))

    assert(boardState.expectingOrders(aNotice.plannedNoticeKey) == Set(aOrderId))
    assert(!boardState.hasNotice(aNotice.plannedNoticeKey))

    boardState = boardState.addExpectation(aNotice.plannedNoticeKey, bOrderId).orThrow
    assert(boardState.toNoticePlace == Map(
      aNotice.plannedNoticeKey -> NoticePlace(
        None,
        Set(aOrderId, bOrderId))))

    assert(boardState.expectingOrders(aNotice.plannedNoticeKey) == Set(aOrderId, bOrderId))
    assert(!boardState.hasNotice(aNotice.plannedNoticeKey))
    assert(boardState.notice(aNotice.plannedNoticeKey).isLeft)
    assert(boardState.notices.isEmpty)

    boardState = boardState.addNotice(aNotice).orThrow
    assert(boardState.toNoticePlace == Map(
      aNotice.plannedNoticeKey -> NoticePlace(
        Some(aNotice),
        Set(aOrderId, bOrderId))))
    assert(boardState.notice(aNotice.plannedNoticeKey) == Right(aNotice))
    assert(boardState.notices.toSeq == Seq(aNotice))

    boardState = boardState.removeExpectation(aNotice.plannedNoticeKey, aOrderId).orThrow
    assert(boardState.toNoticePlace == Map(
      aNotice.plannedNoticeKey -> NoticePlace(
        Some(aNotice),
        Set(bOrderId))))

    boardState = boardState.removeNotice(aNotice.plannedNoticeKey).orThrow
    assert(boardState.toNoticePlace == Map(
      aNotice.plannedNoticeKey -> NoticePlace(
        None,
        Set(bOrderId))))

    boardState = boardState.removeExpectation(aNotice.plannedNoticeKey, bOrderId).orThrow
    assert(boardState.toNoticePlace == Map.empty)

  "addExpectation, removeExpectation (2)" in:
    var boardState = BoardState(board)
    boardState = boardState.addNotice(aNotice).orThrow
    boardState = boardState.addExpectation(aNotice.plannedNoticeKey, aOrderId).orThrow
    assert(boardState.toNoticePlace == Map(
      aNotice.plannedNoticeKey -> NoticePlace(
        Some(aNotice),
        Set(aOrderId))))

    boardState = boardState.removeExpectation(aNotice.plannedNoticeKey, aOrderId).orThrow
    assert(boardState.toNoticePlace == Map(
      aNotice.plannedNoticeKey -> NoticePlace(Some(aNotice))))

  "BoardState snapshot" in:
    for snapshot <- boardState.toSnapshotStream.compile.toVector yield
      assert(snapshot == List(board, notice))

      // Order of addExpectation is irrelevant
      var recovered = BoardState(board)
      recovered = recovered.addNotice(notice).orThrow
      recovered = recovered.addExpectation(GlobalNoticeKey("B"), aOrderId).orThrow
      recovered = recovered.addExpectation(GlobalNoticeKey("B"), bOrderId).orThrow
      assert(recovered == boardState)

      // Now the other way round
      recovered = BoardState(board)
      recovered = recovered.addExpectation(GlobalNoticeKey("B"), bOrderId).orThrow
      recovered = recovered.addExpectation(GlobalNoticeKey("B"), aOrderId).orThrow
      recovered = recovered.addNotice(notice).orThrow
      assert(recovered == boardState)

private object BoardStateTest:

  private val board = GlobalBoard(
    BoardPath("BOARD"),
    postOrderToNoticeKey = expr("'NOTICE'"),
    expectOrderToNoticeKey = expr("'NOTICE'"),
    endOfLife = expr("$js7EpochMilli + 24 * 3600 * 1000"))

  private val endOfLife = Timestamp.ofEpochSecond(1)
  private val notice = Notice(board.path / GlobalNoticeKey("A"), endOfLife.some)
  private val aOrderId = OrderId("A")
  private val bOrderId = OrderId("B")

  private val aNotice = Notice(board.path / GlobalNoticeKey("A"), endOfLife.some)
  private val bNotice = Notice(board.path / GlobalNoticeKey("B"), endOfLife.some)

  private val boardState = BoardState(
    board,
    Map(
      notice.plannedNoticeKey -> NoticePlace(Some(notice)),
      GlobalNoticeKey("B") -> NoticePlace(None, Set(aOrderId, bOrderId))))
