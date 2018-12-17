package com.sos.jobscheduler.master.gui.browser.common

import com.sos.jobscheduler.data.workflow.Instruction
import com.sos.jobscheduler.data.workflow.instructions.ImplicitEnd
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting.{AExecute, BExecute}

/**
  * @author Joacim Zschimmer
  */
final class FlatWorkflowTest extends org.scalatest.FreeSpec
{
  "flattenWorkflow" in {
    assert(FlatWorkflows.flattenWorkflow(ForkTestSetting.TestWorkflow) == Vector[(Position, Instruction.Labeled)](
      (Position(0         ), ForkTestSetting.TestWorkflow.instruction(1)),
      (Position(0, "🥕", 0), AExecute),
      (Position(0, "🥕", 1), ImplicitEnd),
      (Position(0, "🍋", 0), AExecute),
      (Position(0, "🍋", 1), ImplicitEnd),
      (Position(1         ), ForkTestSetting.TestWorkflow.instruction(1)),  // Fork
      (Position(1, "🥕", 0), AExecute),
      (Position(1, "🥕", 1), ImplicitEnd),
      (Position(1, "🍋", 0), AExecute),
      (Position(1, "🍋", 1), ImplicitEnd),
      (Position(2         ), BExecute),
      (Position(3         ), ForkTestSetting.TestWorkflow.instruction(3)),  // Fork
      (Position(3, "🥕", 0), BExecute),
      (Position(3, "🥕", 1), ImplicitEnd),
      (Position(3, "🍋", 0), AExecute),
      (Position(3, "🍋", 1), BExecute),
      (Position(3, "🍋", 2), ImplicitEnd),
      (Position(4         ), ForkTestSetting.TestWorkflow.instruction(4)),  // Fork
      (Position(4, "🥕", 0), AExecute),
      (Position(4, "🥕", 1), ImplicitEnd),
      (Position(4, "🍋", 0), BExecute),
      (Position(4, "🍋", 1), ImplicitEnd),
      (Position(5         ), ImplicitEnd)))
  }
}
