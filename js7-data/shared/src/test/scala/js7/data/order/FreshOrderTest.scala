package js7.data.order

import js7.base.circeutils.CirceUtils.*
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.time.Timestamp
import js7.data.plan.PlanSchemaId
import js7.data.value.{BooleanValue, ListValue, NumberValue, StringValue}
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.{BranchId, Label, Position}
import js7.tester.CirceJsonTester.testJson
/**
  * @author Joacim Zschimmer
  */
final class FreshOrderTest extends OurTestSuite:
  "JSON" in:
    testJson(
      FreshOrder(OrderId("ORDER-ID"), WorkflowPath("WORKFLOW")),
      json"""{
        "id": "ORDER-ID",
        "workflowPath": "WORKFLOW",
        "planId": []
      }""")

    testJson(
      FreshOrder(OrderId("ORDER-ID"), WorkflowPath("WORKFLOW"),
        Map(
          "boolean" -> BooleanValue.True,
          "number" -> NumberValue(BigDecimal("-111222333444555666777888999000111222333444555666777888999000.123")),
          "string" -> StringValue("STRING"),
          "list" -> ListValue(Seq(BooleanValue.True, NumberValue(123), StringValue("string")))),
        PlanSchemaId("DailyPlan") / "2025-02-26",
        Some(Timestamp.parse("2017-03-07T12:00:00Z")),
        deleteWhenTerminated = true,
        forceJobAdmission = true,
        innerBlock = Position(1) / BranchId.Then,
        startPosition = Some(Position(1) / BranchId.Then % 2),
        stopPositions = Set(Position(9) / BranchId.Catch_ % 0, Label("LABEL"))),
      json"""{
        "id": "ORDER-ID",
        "workflowPath": "WORKFLOW",
        "planId" : [ "DailyPlan", "2025-02-26" ],
        "arguments": {
          "boolean": true,
          "number": -111222333444555666777888999000111222333444555666777888999000.123,
          "string": "STRING",
          "list": [ true, 123, "string" ]
        },
        "scheduledFor": 1488888000000,
        "deleteWhenTerminated": true,
        "forceJobAdmission": true,
        "innerBlock": [ 1, "then" ],
        "startPosition": [ 1, "then", 2 ],
        "stopPositions": [ [ 9, "catch", 0 ], "LABEL" ]
      }""")

  "checked" in:
    assert(FreshOrder.checked(OrderId("INVALID|ID"), WorkflowPath("WORKFLOW")) ==
      Left(Problem("OrderId must not contain reserved characters: |")))
