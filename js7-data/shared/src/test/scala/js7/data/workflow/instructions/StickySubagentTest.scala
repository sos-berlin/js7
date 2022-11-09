package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.data.agent.AgentPath
import js7.data.source.SourcePos
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.data.workflow.{Instruction, Workflow}
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class StickySubagentTest extends AnyFreeSpec
{
  "JSON" in {
    testJson[Instruction.Labeled](
      StickySubagent(
        AgentPath("AGENT"),
        None,
        subworkflow = Workflow.empty),
      json"""{
        "TYPE": "StickySubagent",
        "agentPath": "AGENT",
        "subworkflow": {
          "instructions": []
        }
      }""")

    testJson[Instruction.Labeled](
      StickySubagent(
        AgentPath("AGENT"),
        Some(expr("'SUBAGENT-SELECTION'")),
        subworkflow = Workflow.empty,
        Some(SourcePos(1, 2))),
      json"""{
        "TYPE": "StickySubagent",
        "agentPath": "AGENT",
        "subagentSelectionIdExpr": "'SUBAGENT-SELECTION'",
        "subworkflow": {
          "instructions": []
        },
        "sourcePos": [ 1, 2 ]
      }""")
  }
}
