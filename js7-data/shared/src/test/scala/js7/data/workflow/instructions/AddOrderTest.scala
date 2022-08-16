package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils.*
import js7.base.test.Test
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.data.workflow.{Instruction, WorkflowPath}
import js7.tester.CirceJsonTester.testJson

final class AddOrderTest extends Test
{
  "JSON" in {
    testJson[Instruction](
      AddOrder(
        expr("'ORDER-ID'"),
        WorkflowPath("WORKFLOW"),
        Map(
          "arg1" -> expr("7")),
        deleteWhenTerminated = true),
      json"""
      {
        "TYPE": "AddOrder",
        "orderId": "'ORDER-ID'",
        "workflowPath": "WORKFLOW",
        "arguments": {
          "arg1": "7"
        },
        "deleteWhenTerminated": true
      }""")
  }
}
