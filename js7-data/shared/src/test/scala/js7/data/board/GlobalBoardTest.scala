package js7.data.board

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.circeutils.typed.TypedJsonCodec
import js7.base.test.OurTestSuite
import js7.data.item.ItemRevision
import js7.data.value.expression.ExpressionParser.expr
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

final class GlobalBoardTest extends OurTestSuite:

  "JSON" - {
    "standard" in:
      testJson(
        GlobalBoard(
          BoardPath("BOARD"),
          postOrderToNoticeKey =
            expr("""match(orderId, '#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*', '$1')"""),
          expectOrderToNoticeKey =
            expr("""match(orderId, '#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*', '$1')"""),
          endOfLife = expr("$js7EpochMilli + 24 * 3600 * 1000"),
          Some(ItemRevision(7))),
        json"""
          {
            "TYPE": "GlobalBoard",
            "path": "BOARD",
            "postOrderToNoticeKey": "match(orderId, '#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*', '$$1')",
            "endOfLife": "$$js7EpochMilli + 24 * 3600 * 1000",
            "expectOrderToNoticeKey": "match(orderId, '#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*', '$$1')",
            "itemRevision": 7
          }"""
      )(using TypedJsonCodec[GlobalBoard](GlobalBoard.subtype))

    "until v2.7.3" in: // COMPATIBLE with v2.7.3
      testJsonDecoder(
        GlobalBoard(
          BoardPath("BOARD"),
          postOrderToNoticeKey = expr("1"),
          expectOrderToNoticeKey = expr("1"),
          endOfLife = expr("1"),
          Some(ItemRevision(7))),
        json"""
          {
            "TYPE": "GlobalBoard",
            "path": "BOARD",
            "postOrderToNoticeId": "1",
            "endOfLife": "1",
            "expectOrderToNoticeId": "1",
            "itemRevision": 7
          }"""
      )(using TypedJsonCodec[GlobalBoard](GlobalBoard.subtype))

    "until v2.7.2" in: // COMPATIBLE with v2.7.2
      testJsonDecoder(
        GlobalBoard(
          BoardPath("BOARD"),
          postOrderToNoticeKey = expr("1"),
          expectOrderToNoticeKey = expr("1"),
          endOfLife = expr("1"),
          Some(ItemRevision(7))),
        json"""
          {
            "TYPE": "Board",
            "path": "BOARD",
            "postOrderToNoticeId": "1",
            "endOfLife": "1",
            "expectOrderToNoticeId": "1",
            "itemRevision": 7
          }"""
      )(using TypedJsonCodec[GlobalBoard](GlobalBoard.subtype))
  }

  "BoardPath.itemTypeName" in:
    assert(BoardPath.itemTypeName == "Board")
    assert(GlobalBoard.typeName == "GlobalBoard")
