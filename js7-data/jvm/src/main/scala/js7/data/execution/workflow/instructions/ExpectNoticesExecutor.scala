package js7.data.execution.workflow.instructions

import js7.data.workflow.instructions.ExpectNotices

private[instructions] object ExpectNoticesExecutor extends ConsumeOrExpectNoticesExecutor:

  type Instr = ExpectNotices
  val instructionClass = classOf[ExpectNotices]
