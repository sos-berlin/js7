package js7.data.execution.workflow.instructions

import js7.data.workflow.instructions.ExpectNotices

private[instructions] final class ExpectNoticesExecutor(
  protected val service: InstructionExecutorService)
extends
  ConsumeOrExpectNoticesExecutor:

  type Instr = ExpectNotices
  val instructionClass = classOf[ExpectNotices]
