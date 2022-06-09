package js7.data.order

import js7.base.circeutils.CirceUtils._
import js7.base.problem.Problem
import js7.base.time.Timestamp
import js7.data.value.{BooleanValue, ListValue, NumberValue, StringValue}
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.Position
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class FreshOrderTest extends AnyFreeSpec
{
  "JSON" in {
    testJson(
      FreshOrder(OrderId("ORDER-ID"), WorkflowPath("WORKFLOW")),
      json"""{
        "id": "ORDER-ID",
        "workflowPath": "WORKFLOW"
      }""")

    testJson(
      FreshOrder(OrderId("ORDER-ID"), WorkflowPath("WORKFLOW"),
        Map(
          "boolean" -> BooleanValue.True,
          "number" -> NumberValue(BigDecimal("-111222333444555666777888999000111222333444555666777888999000.123")),
          "string" -> StringValue("STRING"),
          "list" -> ListValue(Seq(BooleanValue.True, NumberValue(123), StringValue("string")))),
        Some(Timestamp.parse("2017-03-07T12:00:00Z")),
        deleteWhenTerminated = true,
        startPosition = Some(Position(1)),
        stopPosition = Some(Position(9))),
      json"""{
        "id": "ORDER-ID",
        "workflowPath": "WORKFLOW",
        "scheduledFor": 1488888000000,
        "arguments": {
          "boolean": true,
          "number": -111222333444555666777888999000111222333444555666777888999000.123,
          "string": "STRING",
          "list": [ true, 123, "string" ]
        },
        "deleteWhenTerminated": true,
        "startPosition": [ 1 ],
        "stopPosition": [ 9 ]
      }""")
  }

  "checked" in {
    assert(FreshOrder.checked(OrderId("INVALID|ID"), WorkflowPath("WORKFLOW")) ==
      Left(Problem("OrderId must not contain reserved characters: |")))
  }
}
