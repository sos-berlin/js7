package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.workflow.instructions.Instructions.jsonCodec
import com.sos.jobscheduler.data.workflow.{Instruction, Label}
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class IfNonZeroReturnCodeGotoTest extends FreeSpec {

  // For internal JobScheduler use only.

  "JSON" in {
    testJson[Instruction.Labeled](
      IfNonZeroReturnCodeGoto(Label("A")),
      json"""{
        "TYPE": "IfNonZeroReturnCodeGoto",
        "to": "A"
      }""")
  }
}
