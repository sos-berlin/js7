package js7.data.board

import io.circe.Codec
import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.circeutils.typed.TypedJsonCodec
import js7.base.test.OurTestSuite
import js7.data.item.ItemRevision
import js7.data.value.expression.ExpressionParser.expr
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

final class PlannableBoardTest extends OurTestSuite:

  "JSON" in:
    given Codec[PlannableBoard] = TypedJsonCodec[PlannableBoard](PlannableBoard.subtype)
    testJson(
      PlannableBoard(
        BoardPath("BOARD"),
        postOrderToNoticeKey = expr("1"),
        expectOrderToNoticeKey = expr("2"),
        Some(ItemRevision(7))),
      json"""{
        "TYPE": "PlannableBoard",
        "path": "BOARD",
        "postOrderToNoticeKey": "1",
        "expectOrderToNoticeKey": "2",
        "itemRevision": 7
      }""")

    testJsonDecoder(
      PlannableBoard(
        BoardPath("BOARD"),
        postOrderToNoticeKey = expr("\"\""),
        expectOrderToNoticeKey = expr("\"\"")),
      json"""{
        "TYPE": "PlannableBoard",
        "path": "BOARD"
      }""")

    testJsonDecoder(
      PlannableBoard(BoardPath("BOARD")),
      json"""{
        "TYPE": "PlannableBoard",
        "path": "BOARD"
      }""")

  "BoardPath.itemTypeName" in:
    assert(BoardPath.itemTypeName == "Board")
    assert(PlannableBoard.typeName == "PlannableBoard")
