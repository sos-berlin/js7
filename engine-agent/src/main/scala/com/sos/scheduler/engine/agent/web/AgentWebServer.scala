package com.sos.scheduler.engine.agent.web

import akka.actor.{ActorRef, ActorSystem}
import akka.io.{IO, Tcp}
import akka.pattern.ask
import akka.util.Timeout
import com.google.inject.Injector
import com.sos.scheduler.engine.agent.configuration.AgentConfiguration
import com.sos.scheduler.engine.agent.configuration.AgentConfiguration.Https
import com.sos.scheduler.engine.agent.web.AgentWebServer._
import com.sos.scheduler.engine.agent.web.auth.{SimpleUserPassAuthenticator, UnknownUserPassAuthenticator}
import com.sos.scheduler.engine.common.auth.{Account, UserAndPassword}
import com.sos.scheduler.engine.common.sprayutils.https.Https.newServerSSLEngineProvider
import com.sos.scheduler.engine.common.time.ScalaTime._
import javax.inject.{Inject, Provider, Singleton}
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import spray.can.Http
import spray.can.Http.Unbind
import spray.can.server.ServerSettings
import spray.http.Uri
import spray.io.ServerSSLEngineProvider
import spray.routing.authentication.UserPassAuthenticator

/**
 * @author Joacim Zschimmer
 */
@Singleton
final class AgentWebServer @Inject private(
  conf: AgentConfiguration,
  passwordValidatorProvider: Provider[UserAndPassword ⇒ Boolean],
  injector: Injector)(
  implicit actorSystem: ActorSystem, ec: ExecutionContext)
extends AutoCloseable {

  val localHttpUriOption: Option[Uri] = conf.httpPort map toLocalUri("http")
  val localHttpsUriOption: Option[Uri] = conf.https map { o ⇒ toLocalUri("https")(o.port) }
  val localUri: Uri = localHttpUriOption orElse localHttpsUriOption getOrElse { throw newPortNeededException }

  private def toLocalUri(scheme: String)(port: Int) = {
    val ip = conf.httpInterfaceRestriction getOrElse "127.0.0.1"
    Uri(s"$scheme://$ip:$port/${conf.strippedUriPathPrefix}" stripSuffix "/")
  }

  /**
   * @return Future, completed when Agent has been started and is running.
   */
  def start(): Future[Unit] = {
    val httpBound = for (port ← conf.httpPort) yield bindHttp(port)
    val httpsBound = for (https ← conf.https) yield bindHttps(https)
    val allBound = httpBound ++ httpsBound
    if (allBound.isEmpty)
      Future.failed(newPortNeededException)
    else
      Future.sequence(allBound) map { _ ⇒ () }
  }

  private def bindHttp(port: Int) =
    bind(port, useHttps = false,
      newWebServiceActorRef(s"AgentWebService-http-$port", UnknownUserPassAuthenticator),
      implicitly[ServerSSLEngineProvider])

  private def bindHttps(https: Https) = {
    val authenticator = new SimpleUserPassAuthenticator(passwordValidatorProvider.get())
    bind(https.port, useHttps = true,
      newWebServiceActorRef(s"AgentWebService-https-${https.port}", authenticator),
      newServerSSLEngineProvider(https.keystoreReference))
  }

  private def newWebServiceActorRef(name: String, authenticator: UserPassAuthenticator[Account]) =
    actorSystem.actorOf(WebServiceActor.props(injector, authenticator), name)

  private def bind(port: Int, useHttps: Boolean, actorRef: ActorRef, sslEngineProvider: ServerSSLEngineProvider): Future[Unit] = {
    implicit val timeout: Timeout = 10.seconds
    val interface = conf.httpInterfaceRestriction getOrElse "0.0.0.0"
    val settings = ServerSettings(actorSystem).copy(sslEncryption = useHttps)
    (IO(Http) ? Http.Bind(actorRef, interface = interface, port = port, settings = Some(settings))(sslEngineProvider))
      .map {
        case _: Http.Bound ⇒  // good
        case failed: Tcp.CommandFailed ⇒
          sys.error(s"Binding to TCP port $port failed: $failed. " +
            "Port is possibly in use and not available. Switch on DEBUG-level logging for `akka.io.TcpListener` to log the cause")
            // (Akka 2.3.7) When Akka #13861 should be fixed, replace by actual exception. See https://github.com/akka/akka/issues/13861
    }
  }

  def close() = {
    implicit val timeout = Timeout(ShutdownTimeout.toFiniteDuration)
    val future = for (_ ← IO(Http) ? Unbind(ShutdownTimeout.toConcurrent);
                      _ ← IO(Http) ? Http.CloseAll) yield ()
    // Does not terminate in time !!!  awaitResult(future, ShutdownTimeout)
  }
}

object AgentWebServer {
  private val ShutdownTimeout = 5.s
  private def newPortNeededException = new IllegalArgumentException("HTTP or HTTPS port is needed")
}
