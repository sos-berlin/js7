package com.sos.scheduler.engine.agent.web

import com.google.inject.Injector
import com.sos.scheduler.engine.agent.commandexecutor.CommandExecutor
import com.sos.scheduler.engine.agent.data.commands.Command
import com.sos.scheduler.engine.agent.process.ProcessHandlerView
import com.sos.scheduler.engine.agent.views.AgentOverview
import javax.inject.{Inject, Provider}
import scala.collection.immutable
import spray.routing.HttpServiceActor

/**
 * @author Joacim Zschimmer
 */
// An Actor must not be a singleton!
final class WebServiceActor @Inject private(
  commandExecutor: CommandExecutor,
  agentOverviewProvider: Provider[AgentOverview],
  protected val processHandlerView: ProcessHandlerView,
  webServices: immutable.Seq[WebService],
  injector: Injector)
extends HttpServiceActor
with CommandService
with LegacyCommandService
with ViewService {

  private def standardRoutes = List(agentCommandRoute, legacyCommandRoute, viewRoute)

  def receive = {
    val routes = standardRoutes ++ (webServices map { _.route })
    runRoute((routes :+ reject) reduce { _ ~ _ })
  }

  def executeCommand(command: Command) = commandExecutor.executeCommand(command)

  def agentOverview = agentOverviewProvider.get()
}
