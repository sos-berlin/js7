package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils.*
import js7.data.source.SourcePos
import js7.data.workflow.Instruction
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class FinishTest extends AnyFreeSpec
{
  "JSON" - {
    "with defaults" in {
      testJson[Instruction.Labeled](
        Finish(None),
        json"""{
          "TYPE": "Finish"
        }""")
    }

    "complete" in {
      testJson[Instruction.Labeled](
        Finish(Some(SourcePos(1, 2))),
        json"""{
          "TYPE": "Finish",
          "sourcePos": [ 1, 2 ]
        }""")
    }
  }
}
