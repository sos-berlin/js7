package com.sos.jobscheduler.master.gui.browser.common

import com.sos.jobscheduler.data.workflow.instructions.ImplicitEnd
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting.{AJob, BJob}
import com.sos.jobscheduler.data.workflow.{Instruction, Position}

/**
  * @author Joacim Zschimmer
  */
final class FlatWorkflowTest extends org.scalatest.FreeSpec {

  "flattenWorkflow" in {
    assert(FlatWorkflows.flattenWorkflow(ForkTestSetting.TestWorkflow) == Vector[(Position, Instruction.Labeled)](
      (Position(0         ), AJob),
      (Position(1         ), ForkTestSetting.TestWorkflow.instruction(1)),
      (Position(1, "🥕", 0), AJob),
      (Position(1, "🥕", 1), AJob),
      (Position(1, "🥕", 2), ImplicitEnd),
      (Position(1, "🍋", 0), AJob),
      (Position(1, "🍋", 1), BJob),
      (Position(1, "🍋", 2), ImplicitEnd),
      (Position(2         ), AJob),
      (Position(3         ), ForkTestSetting.TestWorkflow.instruction(3)),
      (Position(3, "🥕", 0), AJob),
      (Position(3, "🥕", 1), AJob),
      (Position(3, "🥕", 2), ImplicitEnd),
      (Position(3, "🍋", 0), AJob),
      (Position(3, "🍋", 1), AJob),
      (Position(3, "🍋", 2), ImplicitEnd),
      (Position(4         ), AJob),
      (Position(5         ), ForkTestSetting.TestWorkflow.instruction(5)),
      (Position(5, "🥕", 0), AJob),
      (Position(5, "🥕", 1), AJob),
      (Position(5, "🥕", 2), ImplicitEnd),
      (Position(5, "🍋", 0), BJob),
      (Position(5, "🍋", 1), BJob),
      (Position(5, "🍋", 2), ImplicitEnd),
      (Position(6         ), AJob),
      (Position(7         ), ImplicitEnd)))
  }
}
