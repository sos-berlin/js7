package com.sos.jobscheduler.core.workflow

import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.utils.ScalaUtils.reuseIfEqual
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.workflow.Instruction.{@:, Labeled}
import com.sos.jobscheduler.data.workflow.Workflow
import com.sos.jobscheduler.data.workflow.instructions.{End, Execute, Fork, Gap, Goto, If, IfNonZeroReturnCodeGoto, TryInstruction}

/**
  * @author Joacim Zschimmer
  */
object Workflows {

  implicit final class ExecutableWorkflow(private val underlying: Workflow) extends AnyVal {
    import underlying._

    def reduceForAgent(agentPath: AgentPath): Workflow =
      reuseIfEqual(underlying, underlying.copy(
        rawLabeledInstructions = labeledInstructions map {
          case labels @: (instr: If) ⇒
            labels @: reuseIfEqual(instr, instr.copy(
              thenWorkflow = instr.thenWorkflow.reduceForAgent(agentPath),
              elseWorkflow = instr.elseWorkflow map (_.reduceForAgent(agentPath))))

          case labels @: (instr: TryInstruction) ⇒
            labels @: reuseIfEqual(instr, instr.copy(
              tryWorkflow = instr.tryWorkflow.reduceForAgent(agentPath),
              catchWorkflow = instr.catchWorkflow.reduceForAgent(agentPath)))

          case labels @: (fj: Fork) if fj isPartiallyExecutableOnAgent agentPath ⇒
            labels @: Fork(
              for (b ← fj.branches) yield
                reuseIfEqual(b, b.copy(
                  workflow = b.workflow.reduceForAgent(agentPath))))

          case o @ _ @: (ex: Execute.Named) if underlying.findJob(ex.name).orThrow/*never*/ isExecutableOnAgent agentPath ⇒
            o

          case o @ _ @: (ex: Execute.Anonymous) if ex.job isExecutableOnAgent agentPath ⇒
            o

          case o @ _ @: (_: End | _: IfNonZeroReturnCodeGoto | _: Goto )  ⇒
            o

          case Labeled(labels, _) ⇒
            Labeled(labels, Gap)
        }))
  }
}
