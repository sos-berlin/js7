package com.sos.scheduler.engine.agent.web

import akka.actor.ActorSystem
import com.google.inject.Injector
import com.sos.scheduler.engine.agent.configuration.AgentConfiguration
import com.sos.scheduler.engine.common.sprayutils.WebServerBinding
import com.sos.scheduler.engine.common.sprayutils.web.SprayWebServer
import com.sos.scheduler.engine.common.sprayutils.web.auth.{CSRF, GateKeeper}
import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext

/**
 * @author Joacim Zschimmer
 */
@Singleton
final class AgentWebServer @Inject private(
  conf: AgentConfiguration,
  gateKeeperConfiguration: GateKeeper.Configuration,
  csrf: CSRF,
  injector: Injector)
  (implicit
    protected val actorSystem: ActorSystem,
    protected val executionContext: ExecutionContext)
extends SprayWebServer with SprayWebServer.HasUri {

  protected val bindings = conf.http ++ conf.https
  protected val uriPathPrefix = conf.uriPathPrefix

  protected def newRouteActorRef(binding: WebServerBinding) =
    actorSystem.actorOf(
      WebServiceActor.props(
        new GateKeeper(gateKeeperConfiguration, csrf, isUnsecuredHttp = binding.isUnsecuredHttp),
        injector),
      name = SprayWebServer.actorName("AgentWebServer", binding))
}
