package com.sos.jobscheduler.master.web

import akka.actor.{ActorRefFactory, ActorSystem}
import akka.http.scaladsl.server.Route
import com.google.inject.Injector
import com.sos.jobscheduler.common.akkahttp.WebLogDirectives
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeper
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.event.collector.EventCollector
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.{FileBasedApi, OrderClient, WorkflowClient}
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
  protected def eventCollector      = injector.instance[EventCollector]
  protected def eventIdGenerator    = injector.instance[EventIdGenerator]
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
    eventIdGenerator: EventIdGenerator,
    actorSystem: ActorSystem,
    scheduler: Scheduler,
    injector: Injector)
  {
    def toRoute(
      gateKeeper: GateKeeper,
      getFileBasedApi: () ⇒ FileBasedApi,
      getWorkflowClient: () ⇒ WorkflowClient,
      getOrderClient: () ⇒ OrderClient,
      execCmd: () ⇒ MasterCommand ⇒ Future[MasterCommand.Response])
    : Route =
      new RouteProvider(gateKeeper, injector) {
        protected val fileBasedApi = getFileBasedApi()
        protected val workflowClient = getWorkflowClient()
        protected val orderClient = getOrderClient()
        protected def orderCountFuture = orderClient.orderCount
        protected def executeCommand(command: MasterCommand) = execCmd()(command)
      }.route(actorSystem)
  }
}
