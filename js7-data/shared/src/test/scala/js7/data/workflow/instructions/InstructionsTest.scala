package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils.*
import js7.base.test.OurTestSuite
import js7.data.workflow.Instruction.*
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.tester.CirceJsonTester.testJson

/**
  * @author Joacim Zschimmer
  */
final class InstructionsTest extends OurTestSuite
 {
   "JSON" - {
    "With Label" in {
      testJson[Labeled](
        "A" @: ExplicitEnd(),
        json"""{
          "label": "A",
          "TYPE": "End"
        }""")
    }

    "Without Label" in {
      testJson[Labeled](
        () @: ExplicitEnd(),
        json"""{
          "TYPE": "End"
        }""")
    }
  }
}
