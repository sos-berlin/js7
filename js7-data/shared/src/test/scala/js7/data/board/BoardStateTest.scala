package js7.data.board

import cats.syntax.option.*
import io.circe.Codec
import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils.{CompactPrinter, JsonStringInterpolator, RichJson, parseJson, reparseJson}
import js7.base.circeutils.typed.TypedJsonCodec
import js7.base.test.OurAsyncTestSuite
import js7.base.time.Timestamp
import js7.base.utils.CatsUtils.Nel
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.board.BoardState.NoticeConsumptionSnapshot
import js7.data.controller.ControllerState
import js7.data.order.OrderId
import js7.data.plan.{PlanId, PlanKey, PlanSchemaId}
import js7.data.value.expression.ExpressionParser.expr
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

final class BoardStateTest extends OurAsyncTestSuite:

  "JSON" - {
    val planSchemaId = PlanSchemaId("DailyPlan")
    val planKey = PlanKey("2024-11-08")
    val planId = planSchemaId / planKey
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
          endOfLife = expr("$js7EpochMilli + 24 * 3600 * 1000")))
      val plannedBoard = PlannedBoard(
        planId / boardPath)

      val snapshots = boardState.toSnapshotStream
        .append(plannedBoard.toSnapshotStream)
        .map(_
          .asJson(using ControllerState.snapshotObjectJsonCodec)
          .compactPrint)
        .map(s => parseJson(s).orThrow)
        .compile
        .toVector

      assert(snapshots == List(
        json"""{
          "TYPE": "GlobalBoard",
          "path": "BOARD",
          "postOrderToNoticeKey":
            "replaceAll($$js7OrderId, '^#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*$$', '$$1')",
          "expectOrderToNoticeKey":
            "replaceAll($$js7OrderId, '^#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*$$', '$$1')",
          "endOfLife": "$$js7EpochMilli + 24 * 3600 * 1000"
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
        orderToConsumptionStack = Map(
          OrderId("A-ORDER") -> Nel.of(
            GlobalNoticeKey("NOTICE-3"),
            GlobalNoticeKey("NOTICE-2"),
            GlobalNoticeKey("NOTICE-1"))))

      lazy val plannedBoard = PlannedBoard(
        planId / boardPath,
        toNoticePlace = Map(
          NoticeKey("NOTICE-1") -> NoticePlace(
            isInConsumption = true),
          NoticeKey("NOTICE-2") -> NoticePlace(
            Some(Notice(
              boardPath / GlobalNoticeKey("NOTICE-2"),
              endOfLife = Timestamp.ofEpochSecond(123).some)),
            expectingOrderIds = Set.empty /*Recovered by Order.ExpectingNotices*/ ,
            consumptionCount = 7)))

      "toSnapshotStream JSON" in:
        val snapshots = boardState.toSnapshotStream
          .map(_
            .asJson(using ControllerState.snapshotObjectJsonCodec)
            .printWith(CompactPrinter))
          .map(s => io.circe.parser.parse(s).orThrow)
          .compile.toVector

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
            "TYPE": "NoticeConsumption",
            "boardPath": "BOARD",
            "orderId": "A-ORDER",
            "plannedNoticeKeyStack": [ "NOTICE-3", "NOTICE-2", "NOTICE-1" ]
          }"""))

      "toSnapshotStream and recoverConsumption" in:
        import scala.language.unsafeNulls

        var recoveredBoardState: BoardState = null
        var recoveredPlannedBoard: PlannedBoard = null
        boardState.toSnapshotStream
          .map: o =>
            reparseJson(o, ControllerState.snapshotObjectJsonCodec).orThrow
          .map:
            case board: GlobalBoard =>
              recoveredBoardState = BoardState(board)
            case snapshot: NoticeConsumptionSnapshot =>
              recoveredBoardState = recoveredBoardState.recoverConsumption(snapshot)
          .compile
          .drain
        assert(recoveredBoardState == boardState)
    }

    "for PlannableBoard" - {
      val planId = PlanId(PlanSchemaId("DailyPlan"), PlanKey("2024-11-08"))
      lazy val boardState = BoardState(
        PlannableBoard(boardPath),
        orderToConsumptionStack = Map(
          OrderId("A-ORDER") -> Nel.of(
            planId / NoticeKey.empty,
            planId / NoticeKey("NOTICE-2"),
            planId / NoticeKey.empty)))

      "toSnapshotStream JSON" in:
        val snapshots = boardState.toSnapshotStream
          .map(_
            .asJson(using ControllerState.snapshotObjectJsonCodec)
            .printWith(CompactPrinter))
          .map(s => io.circe.parser.parse(s).orThrow)
          .compile.toVector

        assert(snapshots == List(
          json"""{
            "TYPE": "PlannableBoard",
            "path": "BOARD",
            "postOrderToNoticeKey" : "\"\"",
            "expectOrderToNoticeKey" : "\"\""
          }""", json"""{
            "TYPE": "NoticeConsumption",
            "orderId": "A-ORDER",
            "boardPath": "BOARD",
            "plannedNoticeKeyStack": [
              [ "DailyPlan", "2024-11-08" ],
              [ "DailyPlan", "2024-11-08", "NOTICE-2" ],
              [ "DailyPlan", "2024-11-08" ]
            ]
          }"""))

      "toSnapshotStream and recoverConsumption" in:
        import scala.language.unsafeNulls

        var recovered: BoardState = null
        boardState.toSnapshotStream
          .map(o =>
            reparseJson(o, ControllerState.snapshotObjectJsonCodec).orThrow)
          .map:
            case board: BoardItem =>
              recovered = BoardState(board)
            case o: NoticeConsumptionSnapshot =>
              recovered = recovered.recoverConsumption(o)
          .compile
          .drain
        assert(recovered == boardState)
    }
  }


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

  private val boardState = BoardState(board)
