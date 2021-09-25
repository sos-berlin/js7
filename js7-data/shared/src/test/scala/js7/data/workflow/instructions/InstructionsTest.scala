package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils._
import js7.data.workflow.Instruction._
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class InstructionsTest extends AnyFreeSpec
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
