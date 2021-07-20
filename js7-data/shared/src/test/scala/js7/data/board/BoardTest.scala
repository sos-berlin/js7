package js7.data.board

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.data.value.expression.ExpressionParser.expr
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class BoardTest extends AnyFreeSpec
{
  "JSON" in {
    testJson(
      Board(
        BoardPath("BOARD"),
        toNotice = expr("""replaceAll($js7OrderId, '^#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*$', '$1')"""),
        readingOrderToNoticeId = expr("""replaceAll($js7OrderId, '^#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*$', '$1')"""),
        endOfLife = expr("$epochMillis + 24 * 3600 * 1000")),
      json"""
        {
          "path": "BOARD",
          "toNotice": "replaceAll($$js7OrderId, '^#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*$$', '$$1')",
          "endOfLife": "$$epochMillis + 24 * 3600 * 1000",
          "readingOrderToNoticeId": "replaceAll($$js7OrderId, '^#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*$$', '$$1')"
        }"""
    )
  }
}
