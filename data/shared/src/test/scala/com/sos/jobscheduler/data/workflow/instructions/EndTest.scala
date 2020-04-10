package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.source.SourcePos
import com.sos.jobscheduler.data.workflow.Instruction
import com.sos.jobscheduler.data.workflow.instructions.Instructions.jsonCodec
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class EndTest extends AnyFreeSpec {

  "JSON" - {
    "with defaults" in {
      testJson[Instruction.Labeled](
        ExplicitEnd(),
        json"""{
          "TYPE": "End"
        }""")
      }

    "complete" in {
      testJson[Instruction.Labeled](
        ExplicitEnd(Some(SourcePos(1, 2))),
        json"""{
          "TYPE": "End",
          "sourcePos": [ 1, 2 ]
        }""")
      }
  }
}
