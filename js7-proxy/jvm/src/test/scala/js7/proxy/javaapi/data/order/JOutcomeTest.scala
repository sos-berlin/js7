package js7.proxy.javaapi.data.order

import js7.data.order.Outcome
import js7.data.value.{BooleanValue, ListValue, NumericValue, StringValue}
import org.scalatest.freespec.AnyFreeSpec

final class JOutcomeTest extends AnyFreeSpec {

  "JOutcome.Succeeded()" in {
    JOutcomeTester.testSucceeded(JOutcome(Outcome.Succeeded()))
  }

  "JOutcome.Succeeded with returnCode" in {
    JOutcomeTester.testSucceededRC1(JOutcome(Outcome.Succeeded(Map(
      "returnCode" -> NumericValue(1),
      "aString" -> StringValue("STRING"),
      "aBoolean" -> BooleanValue(true),
      "aList" -> ListValue(List(NumericValue(1), StringValue("STRING"), BooleanValue(true)))))))
  }
}
