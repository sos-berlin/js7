package js7.subagent.director

import cats.effect.IO
import js7.base.log.Logger
import js7.base.monixutils.Switch
import js7.base.problem.Checked
import js7.base.stream.Numbered
import js7.data.subagent.{SubagentCommand, SubagentId, SubagentRunId}
import js7.subagent.director.SubagentDispatcher.*

private final class SubagentDispatcher(
  subagentId: SubagentId,
  protected val postCommand: PostCommand)
extends CommandDispatcher:
  protected type Command = SubagentCommand.Queueable
  protected type Response = Unit

  protected def name = subagentId.toString
    
  override def toString = s"SubagentDispatcher($name)"


private object SubagentDispatcher:
  type PostCommand = (Numbered[SubagentCommand.Queueable], SubagentRunId, Switch.ReadOnly) =>
    IO[Checked[Unit]]

  private val logger = Logger[this.type]
