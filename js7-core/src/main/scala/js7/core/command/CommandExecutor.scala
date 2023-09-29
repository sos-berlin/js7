package js7.core.command

import js7.base.problem.Checked
import js7.data.command.CommonCommand
import monix.eval.Task

/**
  * @author Joacim Zschimmer
  */
trait CommandExecutor[C <: CommonCommand]:
  def executeCommand(command: C, meta: CommandMeta): Task[Checked[command.Response]]
