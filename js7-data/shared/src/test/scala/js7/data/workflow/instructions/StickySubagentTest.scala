package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.data.agent.AgentPath
import js7.data.source.SourcePos
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.data.workflow.{Instruction, Workflow}
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

final class StickySubagentTest extends OurTestSuite:

  "JSON" in:
    testJson[Instruction.Labeled](
      StickySubagent(AgentPath("AGENT")):
        Workflow.empty,
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
        Some(expr("'SUBAGENT-BUNDLE'")),
        subworkflow = Workflow.empty,
        Some(SourcePos(1, 2))),
      json"""{
        "TYPE": "StickySubagent",
        "agentPath": "AGENT",
        "subagentBundleIdExpr": "'SUBAGENT-BUNDLE'",
        "subworkflow": {
          "instructions": []
        },
        "sourcePos": [ 1, 2 ]
      }""")

  "JSON until v2.7.1" in:
    testJsonDecoder[Instruction.Labeled](
      StickySubagent(AgentPath("AGENT"), Some(expr("'SUBAGENT-BUNDLE'"))):
        Workflow.empty,
      json"""{
        "TYPE": "StickySubagent",
        "agentPath": "AGENT",
        "subagentSelectionIdExpr": "'SUBAGENT-BUNDLE'",
        "subworkflow": {
          "instructions": []
        }
      }""")
