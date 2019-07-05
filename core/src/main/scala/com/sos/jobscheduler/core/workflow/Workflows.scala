package com.sos.jobscheduler.core.workflow

import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.utils.ScalaUtils.reuseIfEqual
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.workflow.Instruction.{@:, Labeled}
import com.sos.jobscheduler.data.workflow.Workflow
import com.sos.jobscheduler.data.workflow.instructions.{End, Execute, Fail, Fork, Gap, Goto, If, IfFailedGoto, Retry, TryInstruction}

/**
  * @author Joacim Zschimmer
  */
object Workflows
{
  implicit final class ExecutableWorkflow(private val underlying: Workflow) extends AnyVal {
    import underlying._

    def reduceForAgent(agentRefPath: AgentRefPath): Workflow =
      reuseIfEqual(underlying, underlying.copy(
        rawLabeledInstructions = labeledInstructions map {
          case labels @: (instr: If) =>
            labels @: reuseIfEqual(instr, instr.copy(
              thenWorkflow = instr.thenWorkflow.reduceForAgent(agentRefPath),
              elseWorkflow = instr.elseWorkflow map (_.reduceForAgent(agentRefPath))))

          case labels @: (instr: TryInstruction) =>
            labels @: reuseIfEqual(instr, instr.copy(
              tryWorkflow = instr.tryWorkflow.reduceForAgent(agentRefPath),
              catchWorkflow = instr.catchWorkflow.reduceForAgent(agentRefPath)))

          case labels @: (fork: Fork) if fork isPartiallyExecutableOnAgent agentRefPath =>
            labels @: fork.copy(
              for (b <- fork.branches) yield
                reuseIfEqual(b, b.copy(
                  workflow = b.workflow.reduceForAgent(agentRefPath))))

          case o @ _ @: (ex: Execute.Named) if underlying.findJob(ex.name).orThrow/*never*/ isExecutableOnAgent agentRefPath =>
            o

          case o @ _ @: (ex: Execute.Anonymous) if ex.job isExecutableOnAgent agentRefPath =>
            o

          case o @ _ @: (_: Fail | _: End | _: IfFailedGoto | _: Goto | _: Retry) =>
            o

          case Labeled(labels, instruction) =>
            Labeled(labels, Gap(instruction.sourcePos))
        }))
  }
}
