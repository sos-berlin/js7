package js7.data.workflow

import js7.base.test.Test
import js7.data.workflow.Instruction.*
import js7.data.workflow.instructions.ExplicitEnd
import js7.data.workflow.position.Label

/**
  * @author Joacim Zschimmer
  */
final class InstructionTest extends Test {

  "@:" in {
    assert("LABEL" @: ExplicitEnd() == Instruction.Labeled(Some("LABEL"), ExplicitEnd()))
    assert(() @: ExplicitEnd() == Instruction.Labeled(None, ExplicitEnd()))
    "LABEL" @: ExplicitEnd() match {
      case Some(Label("LABEL")) @: (_: ExplicitEnd) => // ok
      case _ => fail()
    }
  }
}
