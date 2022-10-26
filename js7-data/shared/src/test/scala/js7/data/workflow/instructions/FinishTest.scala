package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils.*
import js7.base.test.OurTestSuite
import js7.data.order.Outcome
import js7.data.source.SourcePos
import js7.data.workflow.Instruction
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.tester.CirceJsonTester.testJson

/**
  * @author Joacim Zschimmer
  */
final class FinishTest extends OurTestSuite
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
        Finish(Some(Outcome.failed), Some(SourcePos(1, 2))),
        json"""{
          "TYPE": "Finish",
          "outcome": {
            "TYPE": "Failed",
            "namedValues": {}
          },
          "sourcePos": [ 1, 2 ]
        }""")
    }
  }
}
