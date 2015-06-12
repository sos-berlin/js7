package com.sos.scheduler.engine.agent.web

import com.sos.scheduler.engine.agent.commandexecutor.CommandExecutor
import com.sos.scheduler.engine.agent.data.commands.Command
import com.sos.scheduler.engine.agent.process.ProcessHandlerView
import com.sos.scheduler.engine.agent.views.AgentOverview
import javax.inject.{Inject, Provider}
import spray.routing.HttpServiceActor

/**
 * @author Joacim Zschimmer
 */
// No singleton!
final class WebServiceActor @Inject private(
  commandExecutor: CommandExecutor,
  agentOverviewProvider: Provider[AgentOverview],
  protected val processHandlerView: ProcessHandlerView)
extends HttpServiceActor
with CommandService
with LegacyCommandService
with ViewService  {

  def receive = runRoute(agentCommandRoute ~ legacyCommandRoute ~ viewRoute)

  def executeCommand(command: Command) = commandExecutor.executeCommand(command)

  def agentOverview = agentOverviewProvider.get()
}
