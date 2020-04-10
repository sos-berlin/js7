package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.workflow.Instruction._
import com.sos.jobscheduler.data.workflow.Label
import com.sos.jobscheduler.data.workflow.instructions.Instructions.jsonCodec
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class InstructionsTest extends AnyFreeSpec {

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

    "Goto" in {
    }

    "IfFailedGoto" in {
      testJson[Labeled](
        IfFailedGoto(Label("A")),
        json"""{
          "TYPE": "IfFailedGoto",
          "to": "A"
        }""")
    }
  }
}
