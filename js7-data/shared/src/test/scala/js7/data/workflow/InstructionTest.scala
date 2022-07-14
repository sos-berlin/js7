package js7.data.workflow

import js7.data.workflow.Instruction.*
import js7.data.workflow.instructions.ExplicitEnd
import js7.data.workflow.position.Label
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class InstructionTest extends AnyFreeSpec {

  "@:" in {
    assert("LABEL" @: ExplicitEnd() == Instruction.Labeled(Some("LABEL"), ExplicitEnd()))
    assert(() @: ExplicitEnd() == Instruction.Labeled(None, ExplicitEnd()))
    "LABEL" @: ExplicitEnd() match {
      case Some(Label("LABEL")) @: (_: ExplicitEnd) => // ok
      case _ => fail()
    }
  }
}
