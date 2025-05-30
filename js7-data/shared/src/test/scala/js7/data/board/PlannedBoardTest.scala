package js7.data.board

import cats.syntax.option.*
import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils.{CompactPrinter, JsonStringInterpolator, RichJson, parseJson, reparseJson}
import js7.base.test.OurAsyncTestSuite
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.board.PlannedBoardTest.*
import js7.data.controller.ControllerState
import js7.data.order.OrderId
import js7.data.plan.{PlanId, PlanKey, PlanSchemaId}
import js7.data.value.expression.ExpressionParser.expr
import scala.collection.View

final class PlannedBoardTest extends OurAsyncTestSuite:

  "JSON" - {
    val boardPath = BoardPath("BOARD")

    "v2.4 JSON compatibility" in:
      val plannedBoard = PlannedBoard(
        PlanId.Global / BoardPath("BOARD"),
        toNoticePlace = Map(
          NoticeKey("NOTICE") -> NoticePlace(
            Some(Notice(
              boardPath / GlobalNoticeKey("NOTICE"),
              endOfLife = Timestamp.ofEpochSecond(123).some)))))

      val snapshots = plannedBoard.toSnapshotStream
        .map(_
          .asJson(using ControllerState.snapshotObjectJsonCodec)
          .compactPrint)
        .map(s => parseJson(s).orThrow)
        .compile
        .toVector
      assert(snapshots == List(
        json"""{
          "TYPE": "Notice",
          "id": [ "BOARD", "NOTICE" ],
          "endOfLife": 123000
        }"""))

    "for GlobalBoard" - {
      lazy val plannedBoard = PlannedBoard(
        PlanId.Global / boardPath,
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
        val snapshots = plannedBoard.toSnapshotStream
          .map(_
            .asJson(using ControllerState.snapshotObjectJsonCodec)
            .printWith(CompactPrinter))
          .map(s => io.circe.parser.parse(s).orThrow)
          .compile.toVector
        assert(snapshots == List(
          json"""{
            "TYPE": "NoticePlace",
            "noticeId": [ "BOARD" ,"NOTICE-1" ],
            "isAnnounced": false,
            "isInConsumption": true,
            "consumptionCount": 0
          }""", json"""{
            "TYPE": "Notice",
            "id": [ "BOARD" ,"NOTICE-2" ],
            "endOfLife": 123000
          }""", json"""{
            "TYPE": "NoticePlace",
            "noticeId": [ "BOARD", "NOTICE-2" ],
            "isAnnounced": false,
            "isInConsumption": false,
            "consumptionCount": 7
          }"""))

      "toSnapshotStream and recoverConsumption" in:
        var recovered = PlannedBoard(PlanId.Global / board.path)
        plannedBoard.toSnapshotStream
          .map: o =>
            reparseJson(o, ControllerState.snapshotObjectJsonCodec).orThrow
          .map:
            case snapshot: (Notice | NoticePlace.Snapshot) =>
              recovered = recovered.recoverNoticeSnapshot(snapshot).orThrow
          .compile
          .drain
        assert(recovered == plannedBoard)
    }

    "for PlannableBoard" - {
      val planId = PlanId(PlanSchemaId("DailyPlan"), PlanKey("2024-11-08"))
      lazy val plannedBoard = PlannedBoard(
        planId / boardPath,
        toNoticePlace = Map(
          NoticeKey.empty -> NoticePlace(
            isInConsumption = true),
          NoticeKey("NOTICE-2") -> NoticePlace(
            Some(Notice(planId / boardPath / "NOTICE-2")),
            expectingOrderIds = Set.empty /*Recovered by Order.ExpectingNotices*/ ,
            consumptionCount = 7)))

      "toSnapshotStream JSON" in:
        val snapshots = plannedBoard.toSnapshotStream
          .map(_
            .asJson(using ControllerState.snapshotObjectJsonCodec)
            .printWith(CompactPrinter))
          .map(s => io.circe.parser.parse(s).orThrow)
          .compile.toVector
        assert(snapshots == List(
          json"""{
            "TYPE": "NoticePlace",
            "noticeId": [ "DailyPlan", "2024-11-08", "BOARD" ],
            "isAnnounced": false,
            "isInConsumption": true,
            "consumptionCount": 0
          }""", json"""{
            "TYPE": "Notice",
            "id": [ "DailyPlan", "2024-11-08", "BOARD", "NOTICE-2" ]
          }""", json"""{
            "TYPE": "NoticePlace",
            "noticeId": [ "DailyPlan", "2024-11-08", "BOARD", "NOTICE-2" ],
            "isAnnounced": false,
            "isInConsumption": false,
            "consumptionCount": 7
          }"""))

      "toSnapshotStream and recoverConsumption" in:
        var recovered = PlannedBoard(planId / board.path)
        plannedBoard.toSnapshotStream
          .map: o =>
            reparseJson(o, ControllerState.snapshotObjectJsonCodec).orThrow
          .map:
            case snapshot: (Notice | NoticePlace.Snapshot) =>
              recovered = recovered.recoverNoticeSnapshot(snapshot).orThrow
          .compile
          .drain
        assert(recovered == plannedBoard)
    }
  }

  "addNoticeV2_3 (1)" in:
    var boardState = PlannedBoard(PlanId.Global / board.path)
    val aNotice = NoticeV2_3(NoticeKey("A"), endOfLife)

    boardState = boardState.addNoticeV2_3(aNotice).orThrow
    assert(boardState.toNoticePlace == Map(
      aNotice.noticeKey -> NoticePlace(Some(aNotice.toNotice(board.path)))))

    val a1Notice = NoticeV2_3(aNotice.noticeKey, endOfLife)
    boardState = boardState.addNoticeV2_3(a1Notice).orThrow
    assert(boardState.toNoticePlace == Map(
      aNotice.noticeKey -> NoticePlace(Some(a1Notice.toNotice(board.path)))))

    val bNotice = NoticeV2_3(NoticeKey("B"), endOfLife)
    boardState = boardState.addNoticeV2_3(bNotice).orThrow
    assert(boardState.toNoticePlace == Map(
      aNotice.noticeKey -> NoticePlace(Some(aNotice.toNotice(board.path))),
      bNotice.noticeKey -> NoticePlace(Some(bNotice.toNotice(board.path)))))

  "addNoticeV2_3 (2)" in:
    var boardState = PlannedBoard(PlanId.Global / board.path)
    val aNotice = NoticeV2_3(NoticeKey("A"), endOfLife)

    boardState = boardState.addExpectation(aNotice.noticeKey, aOrderId)
    boardState = boardState.addNoticeV2_3(aNotice).orThrow
    assert(boardState.toNoticePlace == Map(
      aNotice.noticeKey -> NoticePlace(
        Some(aNotice.toNotice(board.path)),
        Set(aOrderId))))

  "addNotice, removeNotice (1)" in:
    var boardState = PlannedBoard(PlanId.Global / board.path)
    assert(!boardState.hasNotice(aNotice.noticeKey))

    boardState = boardState.addNotice(aNotice).orThrow
    assert(boardState.toNoticePlace == Map(
      aNotice.noticeKey -> NoticePlace(Some(aNotice))))

    val a1Notice = Notice(PlanId.Global / board.path / aNotice.noticeKey, endOfLife.some)
    boardState = boardState.addNotice(a1Notice).orThrow
    assert(boardState.toNoticePlace == Map(
      aNotice.noticeKey -> NoticePlace(Some(a1Notice))))

    boardState = boardState.addNotice(bNotice).orThrow
    assert(boardState.toNoticePlace == Map(
      aNotice.noticeKey -> NoticePlace(Some(aNotice)),
      bNotice.noticeKey -> NoticePlace(Some(bNotice))))

    assert(boardState.hasNotice(aNotice.noticeKey))
    assert(boardState.hasNotice(bNotice.noticeKey))
    assert(boardState.expectingOrders(aNotice.noticeKey).isEmpty)

    boardState = boardState.removeNotice(aNotice.noticeKey).orThrow
    assert(boardState.toNoticePlace == Map(
      bNotice.noticeKey -> NoticePlace(Some(bNotice))))

    boardState = boardState.removeNotice(bNotice.noticeKey).orThrow
    assert(boardState.toNoticePlace == Map.empty)

  "addNotice, removeNotice (2)" in:
    var boardState = PlannedBoard(PlanId.Global / board.path)
    boardState = boardState.addExpectation(aNotice.noticeKey, aOrderId)
    boardState = boardState.addNotice(aNotice).orThrow
    assert(boardState.toNoticePlace == Map(
      aNotice.noticeKey -> NoticePlace(
        Some(aNotice),
        Set(aOrderId))))

    boardState = boardState.removeNotice(aNotice.noticeKey).orThrow
    assert(boardState.toNoticePlace == Map(
      aNotice.noticeKey -> NoticePlace(
        None,
        Set(aOrderId))))

  "addExpectation, removeExpectation (1)" in:
    var plannedBoard = PlannedBoard(PlanId.Global / board.path)
    plannedBoard = plannedBoard.addExpectation(aNotice.noticeKey, aOrderId)
    assert(plannedBoard.toNoticePlace == Map(
      aNotice.noticeKey -> NoticePlace(
        None,
        Set(aOrderId))))

    assert(plannedBoard.expectingOrders(aNotice.noticeKey) == Set(aOrderId))
    assert(!plannedBoard.hasNotice(aNotice.noticeKey))

    plannedBoard = plannedBoard.addExpectation(aNotice.noticeKey, bOrderId)
    assert(plannedBoard.toNoticePlace == Map(
      aNotice.noticeKey -> NoticePlace(
        None,
        Set(aOrderId, bOrderId))))

    assert(plannedBoard.expectingOrders(aNotice.noticeKey) == Set(aOrderId, bOrderId))
    assert(!plannedBoard.hasNotice(aNotice.noticeKey))
    assert(plannedBoard.notice(aNotice.noticeKey).isLeft)
    assert(plannedBoard.notices.isEmpty)

    plannedBoard = plannedBoard.addNotice(aNotice).orThrow
    assert(plannedBoard.toNoticePlace == Map(
      aNotice.noticeKey -> NoticePlace(
        Some(aNotice),
        Set(aOrderId, bOrderId))))
    assert(plannedBoard.notice(aNotice.noticeKey) == Right(aNotice))
    assert(plannedBoard.notices.toSeq == Seq(aNotice))

    plannedBoard = plannedBoard.removeExpectation(aNotice.noticeKey, aOrderId).orThrow
    assert(plannedBoard.toNoticePlace == Map(
      aNotice.noticeKey -> NoticePlace(
        Some(aNotice),
        Set(bOrderId))))

    plannedBoard = plannedBoard.removeNotice(aNotice.noticeKey).orThrow
    assert(plannedBoard.toNoticePlace == Map(
      aNotice.noticeKey -> NoticePlace(
        None,
        Set(bOrderId))))

    plannedBoard = plannedBoard.removeExpectation(aNotice.noticeKey, bOrderId).orThrow
    assert(plannedBoard.toNoticePlace == Map.empty)

  "addExpectation, removeExpectation (2)" in:
    var boardState = PlannedBoard(PlanId.Global / board.path)
    boardState = boardState.addNotice(aNotice).orThrow
    boardState = boardState.addExpectation(aNotice.noticeKey, aOrderId)
    assert(boardState.toNoticePlace == Map(
      aNotice.noticeKey -> NoticePlace(
        Some(aNotice),
        Set(aOrderId))))

    boardState = boardState.removeExpectation(aNotice.noticeKey, aOrderId).orThrow
    assert(boardState.toNoticePlace == Map(
      aNotice.noticeKey -> NoticePlace(Some(aNotice))))

  "PlannedBoard snapshot" in:
    val snapshot = plannedBoard.toSnapshotStream.compile.toVector
    assert(snapshot == List(notice))

    // Order of addExpectation is irrelevant
    var recovered = PlannedBoard(PlanId.Global / board.path)
    recovered = recovered.addNotice(notice).orThrow
    recovered = recovered.addExpectation(NoticeKey("B"), aOrderId)
    recovered = recovered.addExpectation(NoticeKey("B"), bOrderId)
    assert(recovered == plannedBoard)

    // Now the other way round
    recovered = PlannedBoard(PlanId.Global / board.path)
    recovered = recovered.addExpectation(NoticeKey("B"), bOrderId)
    recovered = recovered.addExpectation(NoticeKey("B"), aOrderId)
    recovered = recovered.addNotice(notice).orThrow
    assert(recovered == plannedBoard)

private object PlannedBoardTest:

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

  private val plannedBoard = PlannedBoard(
    PlanId.Global / board.path,
    Map(
      notice.noticeKey -> NoticePlace(Some(notice)),
      NoticeKey("B") -> NoticePlace(None, Set(aOrderId, bOrderId))))
