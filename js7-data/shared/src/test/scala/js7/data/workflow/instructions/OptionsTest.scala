package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.data.source.SourcePos
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.data.workflow.{Instruction, Workflow}
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class OptionsTest extends AnyFreeSpec
{
  "JSON" in {
    testJson[Instruction.Labeled](
      Options(
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
        block = Workflow.empty),
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
        block = Workflow.empty),
      json"""{
        "TYPE": "Options",
        "stopOnFailure": true,
        "block": {
          "instructions": []
        }
      }""")
  }
}
