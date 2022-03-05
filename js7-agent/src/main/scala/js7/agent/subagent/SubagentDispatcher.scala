package js7.agent.subagent

import js7.agent.subagent.SubagentDispatcher._
import js7.base.monixutils.Switch
import js7.base.problem.Checked
import js7.base.stream.Numbered
import js7.data.subagent.{SubagentId, SubagentRunId}
import js7.subagent.data.SubagentCommand
import monix.eval.Task

final class SubagentDispatcher(
  subagentId: SubagentId,
  protected val postCommand: PostCommand)
extends CommandDispatcher
{
  protected type Command = SubagentCommand.Queueable

  protected def name = subagentId.toString

  override def toString = s"SubagentDispatcher($name)"
}

object SubagentDispatcher
{
  type PostCommand = (Numbered[SubagentCommand.Queueable], SubagentRunId, Switch.ReadOnly) =>
    Task[Checked[Unit]]
}
