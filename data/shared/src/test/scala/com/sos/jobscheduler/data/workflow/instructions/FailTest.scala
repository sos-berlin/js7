package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.expression.Expression.StringConstant
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.source.SourcePos
import com.sos.jobscheduler.data.workflow.Instruction
import com.sos.jobscheduler.data.workflow.instructions.Instructions.jsonCodec
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class FailTest extends AnyFreeSpec
{
  "JSON" - {
    "with defaults" in {
      testJson[Instruction.Labeled](
        Fail(None),
        json"""{
          "TYPE": "Fail"
        }""")
    }

    "complete" in {
      testJson[Instruction.Labeled](
        Fail(Some(StringConstant("ERROR")), Some(ReturnCode(7)), uncatchable = true, Some(SourcePos(1, 2))),
        json"""{
          "TYPE": "Fail",
          "message": "'ERROR'",
          "returnCode": 7,
          "uncatchable": true,
          "sourcePos": [ 1, 2 ]
        }""")
    }
  }
}
