package com.sos.jobscheduler.master.web

import akka.actor.ActorSystem
import com.google.inject.Injector
import com.sos.jobscheduler.base.auth.SimpleUser
import com.sos.jobscheduler.base.convert.AsJava.StringAsPath
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.utils.Closer
import com.sos.jobscheduler.base.utils.Closer.syntax.RichClosersAutoCloseable
import com.sos.jobscheduler.common.akkahttp.web.AkkaWebServer
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeper
import com.sos.jobscheduler.common.akkahttp.web.data.WebServerBinding
import com.sos.jobscheduler.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import com.sos.jobscheduler.common.configutils.Configs._
import com.sos.jobscheduler.common.event.EventWatch
import com.sos.jobscheduler.core.command.CommandMeta
import com.sos.jobscheduler.core.filebased.FileBasedApi
import com.sos.jobscheduler.data.cluster.ClusterState
import com.sos.jobscheduler.master.command.MasterCommandExecutor
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.{MasterState, OrderApi}
import com.typesafe.config.Config
import java.nio.file.Path
import javax.inject.{Inject, Singleton}
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
      protected val currentLogFile = config.as[Path]("jobscheduler.log.file")

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
