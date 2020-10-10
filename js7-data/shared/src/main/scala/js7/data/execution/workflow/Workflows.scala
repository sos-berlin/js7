package js7.data.execution.workflow

import js7.base.problem.Checked.Ops
import js7.base.utils.ScalaUtils.reuseIfEqual
import js7.data.agent.AgentRefPath
import js7.data.workflow.Instruction.{@:, Labeled}
import js7.data.workflow.Workflow
import js7.data.workflow.instructions.{End, Execute, Fail, Fork, Gap, Goto, If, IfFailedGoto, Retry, TryInstruction}

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

          case labels @: instruction =>
            Labeled(labels, Gap(instruction.sourcePos))
        }))
  }
}
