package com.sos.jobscheduler.agent.web

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes.ServiceUnavailable
import com.google.inject.Injector
import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.agent.command.CommandMeta
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.web.AgentWebServer._
import com.sos.jobscheduler.base.auth.SimpleUser
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.common.akkahttp.HttpStatusCodeException
import com.sos.jobscheduler.common.akkahttp.web.AkkaWebServer
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeper
import com.sos.jobscheduler.common.akkahttp.web.data.WebServerBinding
import com.sos.jobscheduler.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.{Closer, Logger, SetOnce}
import monix.execution.Scheduler

/**
 * @author Joacim Zschimmer
 */
final class AgentWebServer(
  conf: AgentConfiguration,
  gateKeeperConfiguration: GateKeeper.Configuration[SimpleUser],
  closer: Closer,
  injector: Injector,
  implicit protected val actorSystem: ActorSystem,
  implicit protected val scheduler: Scheduler)
extends AkkaWebServer with AkkaWebServer.HasUri {

  closer.register(this)

  protected val bindings = conf.webServerBindings.toVector
  private val runningAgentOnce = new SetOnce[RunningAgent]("RunningAgent")

  def setRunningAgent(runningAgent: RunningAgent): Unit =
    this.runningAgentOnce := runningAgent

  private def runningAgent = runningAgentOnce.getOrElse(throw ServiceUnavailableException)

  protected def newRoute(binding: WebServerBinding) =
    new CompleteRoute {
      private lazy val anonymousApi = runningAgent.api(CommandMeta())

      protected implicit def scheduler = AgentWebServer.this.scheduler

      protected val gateKeeper = new GateKeeper(gateKeeperConfiguration,
        isLoopback = binding.address.getAddress.isLoopbackAddress,
        mutual = binding.mutual)
      protected def sessionRegister = injector.instance[SessionRegister[SimpleSession]]

      protected def agentApi(meta: CommandMeta) = runningAgent.api(meta)
      protected def agentOverview = anonymousApi.overview

      protected def commandExecute(meta: CommandMeta, command: AgentCommand) =
        agentApi(meta).commandExecute(command)

      protected def commandOverview = anonymousApi.commandOverview
      protected def commandDetailed = anonymousApi.commandDetailed

      protected def akkaAskTimeout = conf.akkaAskTimeout
      protected def config = AgentWebServer.this.conf.config
      protected def actorSystem = AgentWebServer.this.actorSystem

      logger.info(gateKeeper.boundMessage(binding))
    }.completeRoute
}

object AgentWebServer {
  private val logger = Logger(getClass)

  private val ServiceUnavailableException =
    new HttpStatusCodeException(ServiceUnavailable, Problem("Agent is not yet completely ready"))
}
