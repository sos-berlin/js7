package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.workflow.Instruction
import com.sos.jobscheduler.data.workflow.instructions.Instructions.jsonCodec
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class GapTest extends FreeSpec {

  // For internal JobScheduler use only.

  "JSON" in {
    testJson[Instruction.Labeled](
      Gap,
      json"""{
        "TYPE": "Gap"
      }""")
  }
}
