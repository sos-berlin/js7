package com.sos.scheduler.engine.agent.web

import akka.actor.ActorSystem
import com.google.inject.Injector
import com.sos.scheduler.engine.agent.configuration.AgentConfiguration
import com.sos.scheduler.engine.agent.web.auth.{SimpleUserPassAuthenticator, UnknownUserPassAuthenticator}
import com.sos.scheduler.engine.common.auth.UserAndPassword
import com.sos.scheduler.engine.common.sprayutils.WebServerBinding
import com.sos.scheduler.engine.common.sprayutils.web.SprayWebServer
import javax.inject.{Inject, Provider, Singleton}
import scala.concurrent.ExecutionContext

/**
 * @author Joacim Zschimmer
 */
@Singleton
final class AgentWebServer @Inject private(
  conf: AgentConfiguration,
  passwordValidatorProvider: Provider[UserAndPassword ⇒ Boolean],
  injector: Injector)
  (implicit
    protected val actorSystem: ActorSystem,
    protected val executionContext: ExecutionContext)
extends SprayWebServer with SprayWebServer.HasLocalUri {

  protected val bindings = conf.http ++ conf.https
  protected val uriPathPrefix = conf.uriPathPrefix

  protected def newRouteActorRef(binding: WebServerBinding) =
    actorSystem.actorOf(
      WebServiceActor.props(injector, bindingToAuthenticator(binding)),
      name = SprayWebServer.actorName("AgentWebServer", binding))

  private def bindingToAuthenticator(binding: WebServerBinding) =
    binding match {
      case _: WebServerBinding.Http ⇒ UnknownUserPassAuthenticator
      case _: WebServerBinding.Https ⇒ new SimpleUserPassAuthenticator(passwordValidatorProvider.get())
    }
}
