package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.source.SourcePos
import com.sos.jobscheduler.data.workflow.instructions.Instructions.jsonCodec
import com.sos.jobscheduler.data.workflow.{Instruction, Label}
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class GotoTest extends AnyFreeSpec {

  // For compatibility with JobScheduler 1 only.

  "JSON" in {
    testJson[Instruction.Labeled](
      Goto(Label("A"), Some(SourcePos(1, 2))),
      json"""{
        "TYPE": "Goto",
        "to": "A",
        "sourcePos": [ 1, 2 ]
      }""")
  }
}
