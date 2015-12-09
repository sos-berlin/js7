package com.sos.scheduler.engine.agent.web

import com.google.inject.Injector
import com.sos.scheduler.engine.agent.command.{AgentCommandHandler, CommandExecutor, CommandMeta}
import com.sos.scheduler.engine.agent.configuration.AgentConfiguration
import com.sos.scheduler.engine.agent.data.commands.Command
import com.sos.scheduler.engine.agent.data.views.TaskHandlerView
import com.sos.scheduler.engine.agent.views.AgentOverview
import com.sos.scheduler.engine.agent.web.WebServiceActor._
import com.sos.scheduler.engine.agent.web.common.ExternalWebService
import com.sos.scheduler.engine.agent.web.views.{CommandViewWebService, RootWebService, TaskWebService}
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.time.timer.TimerService
import com.sos.scheduler.engine.tunnel.data.{TunnelId, TunnelToken}
import com.sos.scheduler.engine.tunnel.server.TunnelServer
import java.time.Duration
import javax.inject.{Inject, Provider}
import scala.collection.immutable
import scala.concurrent.ExecutionContext
import spray.routing.HttpServiceActor

/**
 * @author Joacim Zschimmer
 */
// An Actor must not be a singleton!
final class WebServiceActor @Inject private(
  commandExecutor: CommandExecutor,
  tunnelServer: TunnelServer,
  agentOverviewProvider: Provider[AgentOverview],
  protected val taskHandlerView: TaskHandlerView,
  protected val commandHandler: AgentCommandHandler,
  protected val timerService: TimerService,
  extraWebServices: immutable.Seq[ExternalWebService],
  agentConfiguration: AgentConfiguration,
  injector: Injector)
extends HttpServiceActor
with TimerWebService
with CommandWebService
with TunnelWebService
with FileStatusWebService
with RootWebService
with TaskWebService
with CommandViewWebService
with NoJobSchedulerEngineWebService
{
  private lazy val addWebServices = for (o ← extraWebServices) {
    logger.debug(s"Adding extra web service $o")
    addRawRoute(o.route)  // The route is already wrapped, so add it raw, not wrapping it again with agentStandard
  }

  def receive = {
    addWebServices
    runRoute(route)
  }

  protected def commandHandlerOverview = commandHandler
  protected def commandRunOverviews = commandHandler.commandRuns
  protected def executionContext: ExecutionContext = context.dispatcher
  protected def executeCommand(command: Command, meta: CommandMeta) = commandExecutor.executeCommand(command, meta)
  protected def agentOverview = agentOverviewProvider.get()
  protected def tunnelAccess(tunnelToken: TunnelToken) = tunnelServer.tunnelAccess(tunnelToken)
  protected def onTunnelHeartbeat(tunnelToken: TunnelToken, timeout: Duration) = tunnelServer.onHeartbeat(tunnelToken, timeout)
  protected def tunnelHandlerOverview = tunnelServer.overview
  protected def tunnelOverviews = tunnelServer.tunnelOverviews
  protected def tunnelView(tunnelId: TunnelId) = tunnelServer.tunnelView(tunnelId)

  override protected def uriPathPrefix = agentConfiguration.strippedUriPathPrefix
}

object WebServiceActor {
  private val logger = Logger(getClass)
}
