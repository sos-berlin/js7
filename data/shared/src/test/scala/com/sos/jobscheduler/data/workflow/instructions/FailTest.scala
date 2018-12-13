package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.workflow.Instruction
import com.sos.jobscheduler.data.workflow.instructions.Instructions.jsonCodec
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class FailTest extends FreeSpec
{
  "JSON" - {
    "Simple" in {
      testJson[Instruction.Labeled](
        Fail(None),
        json"""{
          "TYPE": "Fail"
        }""")
    }

    "returnCode" in {
      testJson[Instruction.Labeled](
        Fail(Some(ReturnCode(7))),
        json"""{
          "TYPE": "Fail",
          "returnCode":  7
        }""")
    }
  }
}
