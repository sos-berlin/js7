package js7.agent.web

import akka.actor.ActorSystem
import com.typesafe.config.Config
import js7.agent.DirectAgentApi
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.commands.AgentCommand
import js7.base.auth.{SimpleUser, UserId}
import js7.base.generic.Completed
import js7.base.utils.SetOnce
import js7.common.akkahttp.web.AkkaWebServer
import js7.common.akkahttp.web.auth.GateKeeper
import js7.common.akkahttp.web.data.WebServerBinding
import js7.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import js7.core.cluster.ClusterWatchRegister
import js7.core.command.CommandMeta
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.Future
import scala.concurrent.duration.Deadline

/**
 * @author Joacim Zschimmer
 */
final class AgentWebServer(
  conf: AgentConfiguration,
  gateKeeperConfiguration: GateKeeper.Configuration[SimpleUser],
  sessionRegister: SessionRegister[SimpleSession],
  clusterWatchRegister: ClusterWatchRegister,
  protected val config: Config,
  implicit protected val actorSystem: ActorSystem,
  implicit protected val scheduler: Scheduler)
extends AkkaWebServer with AkkaWebServer.HasUri
{
  protected val bindings = conf.webServerBindings
  private val apiOnce = SetOnce[CommandMeta => DirectAgentApi]("api")

  def start(api: CommandMeta => DirectAgentApi): Task[Completed] =
    Task.defer {
      this.apiOnce := api
      super.start
    }

  private def api = apiOnce.orThrow

  protected def newBoundRoute(binding: WebServerBinding, whenTerminating: Future[Deadline]) =
    Task(new AkkaWebServer.BoundRoute with CompleteRoute {
      private lazy val anonymousApi = api(CommandMeta(
        user = gateKeeperConfiguration.idToUser(UserId.Anonymous)
          .getOrElse(sys.error("Anonymous user has not been defined"))))

      protected def whenShuttingDown = whenTerminating
      protected implicit def scheduler: Scheduler = AgentWebServer.this.scheduler

      protected val gateKeeper = GateKeeper(binding, gateKeeperConfiguration)
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
      protected def actorRefFactory = AgentWebServer.this.actorSystem

      def webServerRoute = completeRoute

      override def boundMessageSuffix = gateKeeper.secureStateString
      protected def clusterWatchRegister = AgentWebServer.this.clusterWatchRegister
    })
}
