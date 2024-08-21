package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.data.source.SourcePos
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.data.workflow.{Instruction, Workflow}
import js7.tester.CirceJsonTester.testJson

final class OptionsTest extends OurTestSuite:

  "JSON" in:
    testJson[Instruction.Labeled](
      Options(
        stopOnFailure = None,
        block = Workflow.empty,
        sourcePos = Some(SourcePos(1, 2))),
      json"""{
        "TYPE": "Options",
        "block": {
          "instructions": []
        },
        "sourcePos": [ 1, 2 ]
      }""")

    testJson[Instruction.Labeled](
      Options(
        stopOnFailure = Some(false),
        block = Workflow.empty,
        sourcePos = None),
      json"""{
        "TYPE": "Options",
        "stopOnFailure": false,
        "block": {
          "instructions": []
        }
      }""")

    testJson[Instruction.Labeled](
      Options(
        stopOnFailure = Some(true),
        block = Workflow.empty,
        sourcePos = None),
      json"""{
        "TYPE": "Options",
        "stopOnFailure": true,
        "block": {
          "instructions": []
        }
      }""")
