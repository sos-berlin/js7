package com.sos.scheduler.engine.agent.command

import com.google.inject.ImplementedBy
import com.sos.scheduler.engine.agent.data.commands.Command
import com.sos.scheduler.engine.common.soslicense.LicenseKey
import scala.concurrent.Future

/**
 * @author Joacim Zschimmer
 */
@ImplementedBy(classOf[AgentCommandHandler])
trait CommandExecutor {
  def executeCommand(command: Command, licenseKey: Option[LicenseKey] = None): Future[command.Response]
}
