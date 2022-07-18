package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils.*
import js7.data.value.expression.FastparseExpressionParser.expr
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.data.workflow.{Instruction, WorkflowPath}
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class AddOrderTest extends AnyFreeSpec
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
