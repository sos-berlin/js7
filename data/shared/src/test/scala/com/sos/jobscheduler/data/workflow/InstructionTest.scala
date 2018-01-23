package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.data.workflow.Instruction._
import com.sos.jobscheduler.data.workflow.instructions.ExplicitEnd
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class InstructionTest extends FreeSpec {

  "@:" in {
    assert("LABEL" @: ExplicitEnd == Instruction.Labeled(Label("LABEL") :: Nil, ExplicitEnd))
    assert(() @: ExplicitEnd == Instruction.Labeled(Nil, ExplicitEnd))
    "LABEL" @: ExplicitEnd match {
      case (Label("LABEL") :: Nil) @: ExplicitEnd ⇒ // ok
      case _ ⇒ fail()
    }
  }
}
