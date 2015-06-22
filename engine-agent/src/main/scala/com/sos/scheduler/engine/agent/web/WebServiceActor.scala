package com.sos.scheduler.engine.agent.web

import com.google.inject.Injector
import com.sos.scheduler.engine.agent.command.CommandExecutor
import com.sos.scheduler.engine.agent.data.commands.Command
import com.sos.scheduler.engine.agent.process.ProcessHandlerView
import com.sos.scheduler.engine.agent.views.AgentOverview
import com.sos.scheduler.engine.agent.web.WebServiceActor._
import com.sos.scheduler.engine.agent.web.common.WebService
import com.sos.scheduler.engine.agent.web.views.{MainViewService, ProcessHandlerViewService}
import com.sos.scheduler.engine.common.scalautil.Logger
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
with FileStatusService
with MainViewService
with ProcessHandlerViewService
{
  for (o ← webServices) {
    logger.debug(s"Adding extra web service $o")
    addRawRoute(o.route)  // The route is already wrapped, so add it raw, not wrapping it again with agentStandard
  }

  def receive = runRoute(route)

  def executeCommand(command: Command) = commandExecutor.executeCommand(command)

  def agentOverview = agentOverviewProvider.get()
}

object WebServiceActor {
  private val logger = Logger(getClass)
}
