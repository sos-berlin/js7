package js7.data.board

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.data.item.ItemRevision
import js7.data.value.expression.ExpressionParser.expr
import js7.tester.CirceJsonTester.testJson

final class BoardTest extends OurTestSuite
{
  "JSON" in {
    testJson(
      Board(
        BoardPath("BOARD"),
        postOrderToNoticeId =
          expr("""replaceAll($js7OrderId, '^#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*$', '$1')"""),
        expectOrderToNoticeId =
          expr("""replaceAll($js7OrderId, '^#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*$', '$1')"""),
        endOfLife = expr("$js7EpochMilli + 24 * 3600 * 1000"),
        Some(ItemRevision(7))),
      json"""
        {
          "path": "BOARD",
          "postOrderToNoticeId": "replaceAll($$js7OrderId, '^#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*$$', '$$1')",
          "endOfLife": "$$js7EpochMilli + 24 * 3600 * 1000",
          "expectOrderToNoticeId": "replaceAll($$js7OrderId, '^#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*$$', '$$1')",
          "itemRevision": 7
        }"""
    )
  }

  "BoardPath.itemTypeName" in {
    assert(BoardPath.itemTypeName == Board.typeName)
  }
}
