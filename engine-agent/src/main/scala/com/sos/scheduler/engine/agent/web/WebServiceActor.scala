package com.sos.scheduler.engine.agent.web

import akka.actor.Props
import com.google.inject.Injector
import com.sos.scheduler.engine.agent.command.{AgentCommandHandler, CommandExecutor, CommandMeta}
import com.sos.scheduler.engine.agent.configuration.AgentConfiguration
import com.sos.scheduler.engine.agent.data.commands.Command
import com.sos.scheduler.engine.agent.data.views.TaskHandlerView
import com.sos.scheduler.engine.agent.views.AgentOverview
import com.sos.scheduler.engine.agent.web.WebServiceActor._
import com.sos.scheduler.engine.agent.web.common.ExternalWebService
import com.sos.scheduler.engine.agent.web.views.{CommandViewWebService, RootWebService, TaskWebService}
import com.sos.scheduler.engine.common.guice.GuiceImplicits.RichInjector
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.sprayutils.web.auth.GateKeeper
import com.sos.scheduler.engine.common.time.timer.TimerService
import com.sos.scheduler.engine.tunnel.data.{TunnelId, TunnelToken}
import com.sos.scheduler.engine.tunnel.server.TunnelServer
import java.time.Duration
import javax.inject.{Inject, Provider, Singleton}
import scala.collection.immutable
import scala.concurrent.ExecutionContext
import spray.routing._

/**
 * @author Joacim Zschimmer
 */
// An Actor must not be a @Singleton!
final private class WebServiceActor private(
  gateKeeper: GateKeeper,
  commandExecutor: CommandExecutor,
  tunnelServer: TunnelServer,
  agentOverviewProvider: Provider[AgentOverview],
  protected val taskHandlerView: TaskHandlerView,
  protected val commandHandler: AgentCommandHandler,
  protected val timerService: TimerService,
  extraWebServices: immutable.Seq[ExternalWebService],
  agentConfiguration: AgentConfiguration,
  implicit protected val executionContext: ExecutionContext,
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
  protected val uriPathPrefix = agentConfiguration.uriPathPrefix

  protected def commandHandlerOverview = commandHandler
  protected def commandRunOverviews = commandHandler.commandRuns
  protected def executeCommand(command: Command, meta: CommandMeta) = commandExecutor.executeCommand(command, meta)
  protected def agentOverview = agentOverviewProvider.get()
  protected def tunnelAccess(tunnelToken: TunnelToken) = tunnelServer.tunnelAccess(tunnelToken)
  protected def onTunnelHeartbeat(tunnelToken: TunnelToken, timeout: Duration) = tunnelServer.onHeartbeat(tunnelToken, timeout)
  protected def tunnelHandlerOverview = tunnelServer.overview
  protected def tunnelOverviews = tunnelServer.tunnelOverviews
  protected def tunnelView(tunnelId: TunnelId) = tunnelServer.tunnelView(tunnelId)

  for (o ← extraWebServices) {
    logger.debug(s"Adding extra route $o")
    routeBuilder ++= o.routeBuilder
  }

  def receive = runRoute(buildRoute(gateKeeper))
}

private[web] object WebServiceActor {
  private val logger = Logger(getClass)

  private[web] def props(gateKeeper: GateKeeper, injector: Injector) =
    Props { injector.instance[Factory].apply(gateKeeper) }

  @Singleton
  private class Factory @Inject private(
    commandExecutor: CommandExecutor,
    tunnelServer: TunnelServer,
    agentOverviewProvider: Provider[AgentOverview],
    taskHandlerView: TaskHandlerView,
    commandHandler: AgentCommandHandler,
    timerService: TimerService,
    extraWebServices: immutable.Seq[ExternalWebService],
    agentConfiguration: AgentConfiguration,
    executionContext: ExecutionContext,
    injector: Injector)
  {
    def apply(gateKeeper: GateKeeper) =
      new WebServiceActor(
        gateKeeper,
        commandExecutor,
        tunnelServer,
        agentOverviewProvider,
        taskHandlerView,
        commandHandler,
        timerService,
        extraWebServices,
        agentConfiguration,
        executionContext,
        injector)
  }
}
