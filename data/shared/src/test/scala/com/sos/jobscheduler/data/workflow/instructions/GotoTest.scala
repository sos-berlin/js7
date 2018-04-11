package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.workflow.instructions.Instructions.jsonCodec
import com.sos.jobscheduler.data.workflow.{Instruction, Label}
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class GotoTest extends FreeSpec {

  // For compatibility with JobScheduler 1 only.

  "JSON" in {
    testJson[Instruction.Labeled](
      Goto(Label("A")),
      json"""{
        "TYPE": "Goto",
        "to": "A"
      }""")
  }
}
