package js7.data.board

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.data.value.expression.FastparseExpressionParser.expr
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class BoardTest extends AnyFreeSpec
{
  "JSON" in {
    testJson(
      Board(
        BoardPath("BOARD"),
        postOrderToNoticeId = expr("""replaceAll($js7OrderId, '^#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*$', '$1')"""),
        expectOrderToNoticeId = expr("""replaceAll($js7OrderId, '^#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*$', '$1')"""),
        endOfLife = expr("$js7EpochMilli + 24 * 3600 * 1000")),
      json"""
        {
          "path": "BOARD",
          "postOrderToNoticeId": "replaceAll($$js7OrderId, '^#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*$$', '$$1')",
          "endOfLife": "$$js7EpochMilli + 24 * 3600 * 1000",
          "expectOrderToNoticeId": "replaceAll($$js7OrderId, '^#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*$$', '$$1')"
        }"""
    )
  }

  "BoardPath.itemTypeName" in {
    assert(BoardPath.itemTypeName == Board.typeName)
  }
}
