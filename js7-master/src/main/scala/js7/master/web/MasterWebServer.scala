package js7.master.web

import akka.actor.ActorSystem
import com.google.inject.Injector
import com.typesafe.config.Config
import java.nio.file.Path
import javax.inject.{Inject, Singleton}
import js7.base.auth.SimpleUser
import js7.base.convert.AsJava.StringAsPath
import js7.base.problem.Checked
import js7.base.utils.Closer
import js7.base.utils.Closer.syntax.RichClosersAutoCloseable
import js7.common.akkahttp.web.AkkaWebServer
import js7.common.akkahttp.web.auth.GateKeeper
import js7.common.akkahttp.web.data.WebServerBinding
import js7.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import js7.common.configutils.Configs._
import js7.common.event.EventWatch
import js7.core.command.CommandMeta
import js7.core.filebased.FileBasedApi
import js7.data.cluster.ClusterState
import js7.master.OrderApi
import js7.master.command.MasterCommandExecutor
import js7.master.configuration.MasterConfiguration
import js7.master.data.{MasterCommand, MasterState}
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.duration.Deadline

/**
  * @author Joacim Zschimmer
  */
final class MasterWebServer private(
  masterConfiguration: MasterConfiguration,
  gateKeeperConfiguration: GateKeeper.Configuration[SimpleUser],
  fileBasedApi: FileBasedApi,
  orderApi: OrderApi.WithCommands,
  commandExecutor: MasterCommandExecutor,
  clusterState: Task[ClusterState],
  masterState: Task[Checked[MasterState]],
  totalRunningSince: Deadline,
  sessionRegister: SessionRegister[SimpleSession],
  eventWatch: EventWatch,
  protected val config: Config,
  injector: Injector,
  implicit protected val actorSystem: ActorSystem,
  protected val scheduler: Scheduler)
extends AkkaWebServer with AkkaWebServer.HasUri
{
  protected val bindings = masterConfiguration.webServerBindings

  protected def newRoute(binding: WebServerBinding) =
    new AkkaWebServer.BoundRoute with CompleteRoute {
      protected def isShuttingDown      = MasterWebServer.this.isShuttingDown
      protected val masterConfiguration = MasterWebServer.this.masterConfiguration
      protected val masterId            = masterConfiguration.masterId
      protected val injector            = MasterWebServer.this.injector
      protected val actorSystem         = MasterWebServer.this.actorSystem
      protected implicit def actorRefFactory = MasterWebServer.this.actorSystem
      protected implicit val scheduler  = MasterWebServer.this.scheduler
      protected val config              = MasterWebServer.this.config
      protected val gateKeeper          = new GateKeeper(gateKeeperConfiguration,
        isLoopback = binding.address.getAddress.isLoopbackAddress,
        mutual = binding.mutual)
      protected val sessionRegister     = MasterWebServer.this.sessionRegister
      protected val eventWatch          = MasterWebServer.this.eventWatch
      protected val fileBasedApi = MasterWebServer.this.fileBasedApi
      protected val orderApi = MasterWebServer.this.orderApi
      protected def orderCount = orderApi.orderCount
      protected def executeCommand(command: MasterCommand, meta: CommandMeta) = commandExecutor.executeCommand(command, meta)
      protected def clusterState = MasterWebServer.this.clusterState
      protected def masterState = MasterWebServer.this.masterState
      protected def totalRunningSince = MasterWebServer.this.totalRunningSince
      protected val currentLogFile = config.as[Path]("js7.log.file")

      def webServerRoute = completeRoute

      override def boundMessageSuffix = gateKeeper.secureStateString
    }
}

object MasterWebServer
{
  @Singleton
  final class Factory @Inject private(
    masterConfiguration: MasterConfiguration,
    gateKeeperConfiguration: GateKeeper.Configuration[SimpleUser],
    sessionRegister: SessionRegister[SimpleSession],
    config: Config,
    injector: Injector,
    actorSystem: ActorSystem,
    scheduler: Scheduler,
    closer: Closer)
  {
    def apply(fileBasedApi: FileBasedApi, orderApi: OrderApi.WithCommands,
      commandExecutor: MasterCommandExecutor,
      clusterState: Task[ClusterState], masterState: Task[Checked[MasterState]],
      totalRunningSince: Deadline,
      eventWatch: EventWatch)
    : MasterWebServer =
      new MasterWebServer(
        masterConfiguration, gateKeeperConfiguration,
        fileBasedApi, orderApi, commandExecutor, clusterState, masterState, totalRunningSince,
        sessionRegister, eventWatch, config, injector,
        actorSystem, scheduler)
      .closeWithCloser(closer)
  }
}
