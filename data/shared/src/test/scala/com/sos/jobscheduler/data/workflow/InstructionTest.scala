package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.data.workflow.Instruction._
import com.sos.jobscheduler.data.workflow.instructions.ExplicitEnd
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class InstructionTest extends FreeSpec {

  "@:" in {
    assert("LABEL" @: ExplicitEnd() == Instruction.Labeled(Some("LABEL"), ExplicitEnd()))
    assert(() @: ExplicitEnd() == Instruction.Labeled(None, ExplicitEnd()))
    "LABEL" @: ExplicitEnd() match {
      case Some(Label("LABEL")) @: (_: ExplicitEnd) => // ok
      case _ => fail()
    }
  }
}
