package com.sos.jobscheduler.agent.web

import akka.actor.ActorSystem
import com.google.common.io.Closer
import com.google.inject.Injector
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersCloser
import com.sos.jobscheduler.common.sprayutils.WebServerBinding
import com.sos.jobscheduler.common.sprayutils.web.SprayWebServer
import com.sos.jobscheduler.common.sprayutils.web.auth.{CSRF, GateKeeper}
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
  closer: Closer,
  injector: Injector)
  (implicit
    protected val actorSystem: ActorSystem,
    protected val executionContext: ExecutionContext)
extends SprayWebServer with SprayWebServer.HasUri {

  closer.registerAutoCloseable(this)

  protected val bindings = conf.http ++ conf.https
  protected val uriPathPrefix = conf.uriPathPrefix

  protected def newRouteActorRef(binding: WebServerBinding) =
    actorSystem.actorOf(
      WebServiceActor.props(
        new GateKeeper(gateKeeperConfiguration, csrf, isUnsecuredHttp = binding.isUnsecuredHttp),
        injector),
      name = SprayWebServer.actorName("AgentWebServer", binding))
}
