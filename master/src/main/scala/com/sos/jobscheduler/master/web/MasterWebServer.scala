package com.sos.jobscheduler.master.web

import akka.actor.ActorSystem
import com.google.inject.Injector
import com.sos.jobscheduler.common.akkahttp.WebServerBinding
import com.sos.jobscheduler.common.akkahttp.web.AkkaWebServer
import com.sos.jobscheduler.common.akkahttp.web.auth.{CSRF, GateKeeper}
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext

/**
  * @author Joacim Zschimmer
  */
@Singleton
final class MasterWebServer @Inject private(
  masterConfiguration: MasterConfiguration,
  gateKeeperConfiguration: GateKeeper.Configuration,
  csrf: CSRF,
  timerService: TimerService,
  injector: Injector)
  (implicit
    protected val actorSystem: ActorSystem,
    protected val executionContext: ExecutionContext)
extends AkkaWebServer with AkkaWebServer.HasUri {

  protected def uriPathPrefix = ""
  protected val bindings = masterConfiguration.webServerBindings

  protected def newRoute(binding: WebServerBinding) =
    new RouteProvider(
      new GateKeeper(gateKeeperConfiguration, csrf, timerService, isUnsecuredHttp = binding.isUnsecuredHttp),
      actorSystem,
      injector)
    .route
}
