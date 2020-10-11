package js7.data.execution.workflow

import js7.base.problem.Checked.Ops
import js7.base.utils.ScalaUtils.reuseIfEqual
import js7.data.agent.AgentName
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

    def reduceForAgent(agentName: AgentName): Workflow =
      reuseIfEqual(underlying, underlying.copy(
        rawLabeledInstructions = labeledInstructions map {
          case labels @: (instr: If) =>
            labels @: reuseIfEqual(instr, instr.copy(
              thenWorkflow = instr.thenWorkflow.reduceForAgent(agentName),
              elseWorkflow = instr.elseWorkflow map (_.reduceForAgent(agentName))))

          case labels @: (instr: TryInstruction) =>
            labels @: reuseIfEqual(instr, instr.copy(
              tryWorkflow = instr.tryWorkflow.reduceForAgent(agentName),
              catchWorkflow = instr.catchWorkflow.reduceForAgent(agentName)))

          case labels @: (fork: Fork) if fork isPartiallyExecutableOnAgent agentName =>
            labels @: fork.copy(
              for (b <- fork.branches) yield
                reuseIfEqual(b, b.copy(
                  workflow = b.workflow.reduceForAgent(agentName))))

          case o @ _ @: (ex: Execute.Named) if underlying.findJob(ex.name).orThrow/*never*/ isExecutableOnAgent agentName =>
            o

          case o @ _ @: (ex: Execute.Anonymous) if ex.job isExecutableOnAgent agentName =>
            o

          case o @ _ @: (_: Fail | _: End | _: IfFailedGoto | _: Goto | _: Retry) =>
            o

          case labels @: instruction =>
            Labeled(labels, Gap(instruction.sourcePos))
        }))
  }
}
