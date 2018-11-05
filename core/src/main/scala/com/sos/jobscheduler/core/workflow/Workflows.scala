package com.sos.jobscheduler.core.workflow

import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.workflow.Instruction.{@:, Labeled}
import com.sos.jobscheduler.data.workflow.Workflow
import com.sos.jobscheduler.data.workflow.instructions.{End, Execute, ForkJoin, Gap, Goto, If, IfNonZeroReturnCodeGoto}

/**
  * @author Joacim Zschimmer
  */
object Workflows {

  implicit final class ExecutableWorkflow(private val underlying: Workflow) extends AnyVal {
    import underlying._

    def reduceForAgent(agentPath: AgentPath): Workflow =
      underlying.copy(
        rawLabeledInstructions = labeledInstructions map {
          case labels @: (instr: If) ⇒
            labels @: instr.copy(
              thenWorkflow = instr.thenWorkflow.reduceForAgent(agentPath),
              elseWorkflow = instr.elseWorkflow map (_.reduceForAgent(agentPath)))

          case labels @: (fj: ForkJoin) if fj isPartiallyExecutableOnAgent agentPath ⇒
            labels @: ForkJoin(
              for (b ← fj.branches) yield
                b.copy(workflow = b.workflow.reduceForAgent(agentPath)))

          case o @ _ @: (ex: Execute.Named) if underlying.findJob(ex.name).orThrow/*never*/ isExecutableOnAgent agentPath ⇒
            o

          case o @ _ @: (ex: Execute.Anonymous) if ex.job isExecutableOnAgent agentPath ⇒
            o

          case o @ _ @: (_: End | _: IfNonZeroReturnCodeGoto | _: Goto )  ⇒
            o

          case Labeled(labels, _) ⇒
            Labeled(labels, Gap)
        })
  }
}
