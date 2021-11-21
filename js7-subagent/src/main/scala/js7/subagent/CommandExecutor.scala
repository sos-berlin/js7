package js7.subagent

import js7.base.problem.Checked
import js7.base.stream.Numbered
import js7.subagent.data.SubagentCommand
import monix.eval.Task

trait CommandExecutor
{
  def executeCommand(command: Numbered[SubagentCommand]): Task[Checked[command.value.Response]]
}
