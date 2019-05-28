package com.sos.jobscheduler.agent.web

import akka.actor.ActorSystem
import com.sos.jobscheduler.agent.DirectAgentApi
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.base.auth.SimpleUser
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.common.akkahttp.web.AkkaWebServer
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeper
import com.sos.jobscheduler.common.akkahttp.web.data.WebServerBinding
import com.sos.jobscheduler.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import com.sos.jobscheduler.common.scalautil.SetOnce
import com.sos.jobscheduler.core.command.CommandMeta
import com.typesafe.config.Config
import monix.execution.Scheduler
import scala.concurrent.Future

/**
 * @author Joacim Zschimmer
 */
final class AgentWebServer(
  conf: AgentConfiguration,
  gateKeeperConfiguration: GateKeeper.Configuration[SimpleUser],
  sessionRegister: SessionRegister[SimpleSession],
  protected val config: Config,
  implicit protected val actorSystem: ActorSystem,
  implicit protected val scheduler: Scheduler)
extends AkkaWebServer with AkkaWebServer.HasUri
{
  protected val bindings = conf.webServerBindings
  private val apiOnce = new SetOnce[CommandMeta => DirectAgentApi]("api")

  def start(api: CommandMeta => DirectAgentApi): Future[Completed] = {
    this.apiOnce := api
    super.start()
  }

  private def api = apiOnce()

  protected def newRoute(binding: WebServerBinding) =
    new AkkaWebServer.BoundRoute with CompleteRoute {
      private lazy val anonymousApi = api(CommandMeta.Anonymous)

      protected implicit def scheduler: Scheduler = AgentWebServer.this.scheduler

      protected val gateKeeper = new GateKeeper(gateKeeperConfiguration,
        isLoopback = binding.address.getAddress.isLoopbackAddress,
        mutual = binding.mutual)
      protected def sessionRegister = AgentWebServer.this.sessionRegister

      protected def agentApi(meta: CommandMeta) = api(meta)
      protected def agentOverview = anonymousApi.overview

      protected def commandExecute(meta: CommandMeta, command: AgentCommand) =
        agentApi(meta).commandExecute(command)

      protected def commandOverview = anonymousApi.commandOverview
      protected def commandDetailed = anonymousApi.commandDetailed

      protected def akkaAskTimeout = conf.akkaAskTimeout
      protected def config = AgentWebServer.this.conf.config
      protected def actorSystem = AgentWebServer.this.actorSystem

      def webServerRoute = completeRoute

      override def boundMessageSuffix = gateKeeper.secureStateString
    }
}
