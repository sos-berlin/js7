package com.sos.jobscheduler.agent.web

import akka.actor.ActorSystem
import com.google.common.io.Closer
import com.google.inject.Injector
import com.sos.jobscheduler.agent.command.CommandHandler
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.scheduler.AgentHandle
import com.sos.jobscheduler.agent.web.AgentWebServer._
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersCloser
import com.sos.jobscheduler.common.scalautil.SetOnce
import com.sos.jobscheduler.common.sprayutils.web.SprayWebServer
import com.sos.jobscheduler.common.sprayutils.web.auth.{CSRF, GateKeeper}
import com.sos.jobscheduler.common.sprayutils.{HttpStatusCodeException, WebServerBinding}
import scala.concurrent.ExecutionContext
import spray.http.StatusCodes.ServiceUnavailable

/**
 * @author Joacim Zschimmer
 */
final class AgentWebServer(
  conf: AgentConfiguration,
  gateKeeperConfiguration: GateKeeper.Configuration,
  csrf: CSRF,
  closer: Closer,
  injector: Injector)
  (implicit
    protected val actorSystem: ActorSystem,
    protected val executionContext: ExecutionContext)
extends SprayWebServer with SprayWebServer.HasUri {

  closer.registerAutoCloseable(this)

  protected val bindings = conf.http ++ conf.https
  protected val uriPathPrefix = conf.uriPathPrefix
  private val commandHandler = new SetOnce[CommandHandler]("CommandHandler")
  private val agentHandle = new SetOnce[AgentHandle]("Agent")

  def setCommandHandler(commandHandler: CommandHandler) =
    this.commandHandler := commandHandler

  def setAgentActor(agentHandle: AgentHandle) =
    this.agentHandle := agentHandle

  protected def newRouteActorRef(binding: WebServerBinding) =
    actorSystem.actorOf(
      WebServiceActor.props(
        new GateKeeper(gateKeeperConfiguration, csrf, isUnsecuredHttp = binding.isUnsecuredHttp),
        () ⇒ commandHandler getOrElse newServiceUnavailable(),
        () ⇒ agentHandle getOrElse newServiceUnavailable(),
        injector),
      name = SprayWebServer.actorName("AgentWebServer", binding))
}

object AgentWebServer {
  private def newServiceUnavailable() =
    throw new HttpStatusCodeException(ServiceUnavailable, "Agent CommandHandler is not yet ready")
}
