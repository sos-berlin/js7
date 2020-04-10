package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.source.SourcePos
import com.sos.jobscheduler.data.workflow.Instruction
import com.sos.jobscheduler.data.workflow.instructions.Instructions.jsonCodec
import com.sos.jobscheduler.tester.CirceJsonTester._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class RetryTest extends AnyFreeSpec
{
  "JSON" in {
    testJson[Instruction.Labeled](Retry(Some(SourcePos(1, 2))),
      json"""{
        "TYPE": "Retry",
        "sourcePos": [ 1, 2 ]
      }""")
  }
}
