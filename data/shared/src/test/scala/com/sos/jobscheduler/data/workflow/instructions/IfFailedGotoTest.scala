package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.source.SourcePos
import com.sos.jobscheduler.data.workflow.instructions.Instructions.jsonCodec
import com.sos.jobscheduler.data.workflow.{Instruction, Label}
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class IfFailedGotoTest extends FreeSpec {

  // For internal JobScheduler use only.

  "JSON" in {
    testJson[Instruction.Labeled](
      IfFailedGoto(Label("A"), Some(SourcePos(1, 2))),
      json"""{
        "TYPE": "IfFailedGoto",
        "to": "A",
        "sourcePos": [ 1, 2 ]
      }""")
  }
}
