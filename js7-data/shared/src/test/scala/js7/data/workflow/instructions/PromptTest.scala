package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils.*
import js7.data.source.SourcePos
import js7.data.value.expression.Expression.StringConstant
import js7.data.workflow.Instruction
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class PromptTest extends AnyFreeSpec
{
  "JSON" - {
    "with defaults" in {
      testJson[Instruction.Labeled](
        Prompt(StringConstant("QUESTION")),
        json"""{
          "TYPE": "Prompt",
          "question": "'QUESTION'"
        }""")
    }

    "complete" in {
      testJson[Instruction.Labeled](
        Prompt(StringConstant("QUESTION"), Some(SourcePos(1, 2))),
        json"""{
          "TYPE": "Prompt",
          "question": "'QUESTION'",
          "sourcePos": [ 1, 2 ]
        }""")
    }
  }
}
