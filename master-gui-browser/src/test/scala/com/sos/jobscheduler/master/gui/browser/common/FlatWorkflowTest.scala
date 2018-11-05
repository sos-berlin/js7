package com.sos.jobscheduler.master.gui.browser.common

import com.sos.jobscheduler.data.workflow.Instruction
import com.sos.jobscheduler.data.workflow.instructions.ImplicitEnd
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting.{AExecute, BExecute}

/**
  * @author Joacim Zschimmer
  */
final class FlatWorkflowTest extends org.scalatest.FreeSpec {

  "flattenWorkflow" in {
    assert(FlatWorkflows.flattenWorkflow(ForkTestSetting.TestWorkflow) == Vector[(Position, Instruction.Labeled)](
      (Position(0         ), AExecute),
      (Position(1         ), ForkTestSetting.TestWorkflow.instruction(1)),
      (Position(1, "🥕", 0), AExecute),
      (Position(1, "🥕", 1), AExecute),
      (Position(1, "🥕", 2), ImplicitEnd),
      (Position(1, "🍋", 0), AExecute),
      (Position(1, "🍋", 1), BExecute),
      (Position(1, "🍋", 2), ImplicitEnd),
      (Position(2         ), AExecute),
      (Position(3         ), ForkTestSetting.TestWorkflow.instruction(3)),
      (Position(3, "🥕", 0), AExecute),
      (Position(3, "🥕", 1), AExecute),
      (Position(3, "🥕", 2), ImplicitEnd),
      (Position(3, "🍋", 0), AExecute),
      (Position(3, "🍋", 1), AExecute),
      (Position(3, "🍋", 2), ImplicitEnd),
      (Position(4         ), AExecute),
      (Position(5         ), ForkTestSetting.TestWorkflow.instruction(5)),
      (Position(5, "🥕", 0), AExecute),
      (Position(5, "🥕", 1), AExecute),
      (Position(5, "🥕", 2), ImplicitEnd),
      (Position(5, "🍋", 0), BExecute),
      (Position(5, "🍋", 1), BExecute),
      (Position(5, "🍋", 2), ImplicitEnd),
      (Position(6         ), AExecute),
      (Position(7         ), ImplicitEnd)))
  }
}
