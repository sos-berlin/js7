package js7.data_for_java.order

import js7.base.test.OurTestSuite
import js7.data.order.OrderOutcome
import js7.data.value.{BooleanValue, ListValue, NumberValue, StringValue}

final class JOutcomeTest extends OurTestSuite:

  "JOutcome.Succeeded()" in:
    JOutcomeTester.testSucceeded(JOutcome(OrderOutcome.Succeeded()))

  "JOutcome.Succeeded with returnCode" in:
    JOutcomeTester.testSucceededRC1(JOutcome(OrderOutcome.Succeeded(Map(
      "returnCode" -> NumberValue(1),
      "aString" -> StringValue("STRING"),
      "aBoolean" -> BooleanValue(true),
      "aList" -> ListValue(List(NumberValue(1), StringValue("STRING"), BooleanValue(true)))))))
