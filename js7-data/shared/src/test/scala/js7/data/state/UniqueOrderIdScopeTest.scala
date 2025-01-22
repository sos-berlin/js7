package js7.data.state

import js7.base.log.Logger
import js7.base.test.OurTestSuite
import js7.base.time.Stopwatch.measureTime
import js7.base.utils.Tests
import js7.data.order.OrderId
import js7.data.state.UniqueOrderIdScopeTest.*
import js7.data.value.StringValue
import js7.data.value.expression.ExpressionParser.expr

final class UniqueOrderIdScopeTest extends OurTestSuite:

  "uniqueOrderId" in:
    val scope = UniqueOrderIdScope:
      Set(OrderId("ORDER-001"), OrderId("ORDER-002"), OrderId("ORDER-003"))
    assert(expr(""" uniqueOrderId("ORDER-%03d") """).eval(scope) == Right(StringValue("ORDER-004")))

  "Speed tests" - {
    val n = if Tests.isIntelliJIdea then 1_000_000 else 100_000

    "ORDER-%d, pattern ends with %d" in:
      val idToX = (1 to n).map(i => OrderId(s"ORDER-$i") -> ()).toMap
      val scope = UniqueOrderIdScope(idToX.keySet)
      val uniqueOrderId = expr(""" uniqueOrderId("ORDER-%d") """)
      assert(uniqueOrderId.eval(scope) == Right(StringValue(s"ORDER-${n + 1}")))

      val result = measureTime(n = 10, s"ORDER-%d×$n", warmUp = 0):
        uniqueOrderId.eval(scope)
      logger.info(s"uniqueOrderId: $result")
      note(s"uniqueOrderId: $result")

    "ORDER-%d-X (random pattern)" in:
      val idToX = (1 to n).map(i => OrderId(s"ORDER-$i-X") -> ()).toMap
      val scope = UniqueOrderIdScope(idToX.keySet)
      val uniqueOrderId = expr(""" uniqueOrderId("ORDER-%d-X") """)
      assert(uniqueOrderId.eval(scope) == Right(StringValue(s"ORDER-${n + 1}-X")))

      val result = measureTime(n = 10, s"ORDER-%d-X×$n", warmUp = 0):
        uniqueOrderId.eval(scope)
      logger.info(s"uniqueOrderId: $result")
      note(s"uniqueOrderId: $result")
  }


object UniqueOrderIdScopeTest:
  private val logger = Logger[this.type]
