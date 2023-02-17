package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils.*
import js7.base.test.OurTestSuite
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.data.workflow.position.{Label, Position}
import js7.data.workflow.{Instruction, WorkflowPath}
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

final class AddOrderTest extends OurTestSuite
{
  "JSON" in {
    testJson[Instruction](
      AddOrder(
        expr("'ORDER-ID'"),
        WorkflowPath("WORKFLOW"),
        Map(
          "arg1" -> expr("7")),
        startPosition = Some(Position(1)),
        stopPositions = Set(Position(2), Label("LABEL")),
        deleteWhenTerminated = true),
      json"""
      {
        "TYPE": "AddOrder",
        "orderId": "'ORDER-ID'",
        "workflowPath": "WORKFLOW",
        "arguments": {
          "arg1": "7"
        },
        "startPosition": [1],
        "stopPositions": [ [2], "LABEL"],
        "deleteWhenTerminated": true
      }""")

    testJsonDecoder[Instruction](
      AddOrder(expr("'ORDER-ID'"), WorkflowPath("WORKFLOW")),
      json"""
      {
        "TYPE": "AddOrder",
        "orderId": "'ORDER-ID'",
        "workflowPath": "WORKFLOW",
        "deleteWhenTerminated": false
      }""")
  }
}
