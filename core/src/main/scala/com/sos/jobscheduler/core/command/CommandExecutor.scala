package com.sos.jobscheduler.core.command

import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.data.command.CommonCommand
import monix.eval.Task

/**
  * @author Joacim Zschimmer
  */
trait CommandExecutor[C <: CommonCommand]
{
  def executeCommand(command: C, meta: CommandMeta): Task[Checked[command.Response]]
}
