package com.sos.jobscheduler.master.web

import akka.actor.{ActorRefFactory, ActorSystem}
import akka.http.scaladsl.server.Route
import com.google.inject.Injector
import com.sos.jobscheduler.common.akkahttp.WebLogDirectives
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeper
import com.sos.jobscheduler.common.event.EventReader
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.core.filebased.FileBasedApi
import com.sos.jobscheduler.data.event.Event
import com.sos.jobscheduler.master.OrderApi
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.data.MasterCommand
import com.typesafe.config.Config
import javax.inject.{Inject, Singleton}
import monix.execution.Scheduler
import scala.concurrent.{ExecutionContext, Future}

/**
  * @author Joacim Zschimmer
  */
private abstract class RouteProvider(gateKeeper: GateKeeper, injector: Injector)
extends AllRoute {

  protected val actorSystem = injector.instance[ActorSystem]
  protected def actorRefFactory = actorSystem
  protected val masterConfiguration = injector.instance[MasterConfiguration]
  protected val config              = injector.instance[Config]
  protected def eventReader         = injector.instance[EventReader[Event]]
  protected val executionContext    = injector.instance[ExecutionContext]
  protected val scheduler           = injector.instance[Scheduler]

  def route(implicit actorRefFactory: ActorRefFactory): Route =
    WebLogDirectives(config, actorSystem).handleErrorAndLog {
      gateKeeper.restrict.apply { _ ⇒
        allRoutes
      }
    }
}

object RouteProvider {
  @Singleton
  final class Factory @Inject private(
    //sessionRegister: SessionRegister[LoginSession],
    timerService: TimerService,
    masterConfiguration: MasterConfiguration,
    actorSystem: ActorSystem,
    scheduler: Scheduler,
    injector: Injector)
  {
    def toRoute(
      gateKeeper: GateKeeper,
      getFileBasedApi: () ⇒ FileBasedApi,
      getOrderApi: () ⇒ OrderApi.WithCommands,
      execCmd: () ⇒ MasterCommand ⇒ Future[MasterCommand.Response])
    : Route =
      new RouteProvider(gateKeeper, injector) {
        protected val fileBasedApi = getFileBasedApi()
        protected val orderApi = getOrderApi()
        protected def orderCountFuture = orderApi.orderCount.runAsync(scheduler)
        protected def executeCommand(command: MasterCommand) = execCmd()(command)
      }.route(actorSystem)
  }
}
