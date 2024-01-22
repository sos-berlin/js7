package js7.core.command

import cats.effect.IO
import js7.base.problem.Checked
import js7.data.command.CommonCommand

/**
  * @author Joacim Zschimmer
  */
trait CommandExecutor[C <: CommonCommand]:
  def executeCommand(command: C, meta: CommandMeta): IO[Checked[command.Response]]
