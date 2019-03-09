package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.workflow.Instruction._
import com.sos.jobscheduler.data.workflow.Label
import com.sos.jobscheduler.data.workflow.instructions.Instructions.jsonCodec
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class InstructionsTest extends FreeSpec {

  "JSON" - {
    "With Label" in {
      testJson[Labeled](
        "A" @: ExplicitEnd(),
        json"""{
          "TYPE": "End",
          "labels": [ "A" ]
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

    "IfNonZeroReturnCodeGoto" in {
      testJson[Labeled](
        IfNonZeroReturnCodeGoto(Label("A")),
        json"""{
          "TYPE": "IfNonZeroReturnCodeGoto",
          "to": "A"
        }""")
    }
  }
}
