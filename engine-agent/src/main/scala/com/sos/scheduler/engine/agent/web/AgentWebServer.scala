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
import com.sos.scheduler.engine.base.utils.ScalaUtils
import com.sos.scheduler.engine.base.utils.ScalaUtils.RichAny
import com.sos.scheduler.engine.common.auth.{Account, UserAndPassword}
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.sprayutils.https.Https.newServerSSLEngineProvider
import com.sos.scheduler.engine.common.time.ScalaTime._
import java.net.InetSocketAddress
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

  val localHttpUriOption: Option[Uri] = conf.httpAddress map toLocalUri("http")
  val localHttpsUriOption: Option[Uri] = conf.https map { o ⇒ toLocalUri("https")(o.address) }
  val localUri: Uri = localHttpUriOption orElse localHttpsUriOption getOrElse { throw newPortNeededException }

  private def toLocalUri(scheme: String)(address: InetSocketAddress) = {
    val host = address.getAddress.getHostAddress.substitute("0.0.0.0", "127.0.0.1")
    val port = address.getPort
    Uri(s"$scheme://$host:$port/${conf.uriPathPrefix}" stripSuffix "/")
  }

  /**
   * @return Future, completed when Agent has been started and is running.
   */
  def start(): Future[Unit] = {
    val httpBound = for (port ← conf.httpAddress) yield bindHttp(port)
    val httpsBound = for (https ← conf.https) yield bindHttps(https)
    val allBound = httpBound ++ httpsBound
    if (allBound.isEmpty)
      Future.failed(newPortNeededException)
    else
      Future.sequence(allBound) map { _ ⇒ () }
  }

  private def bindHttp(address: InetSocketAddress) =
    bind(address, useHttps = false,
      newWebServiceActorRef(s"AgentWebService-http-${inetSocketAddressToName(address)}", UnknownUserPassAuthenticator),
      implicitly[ServerSSLEngineProvider])

  private def bindHttps(https: Https) = {
    val authenticator = new SimpleUserPassAuthenticator(passwordValidatorProvider.get())
    logger.info(s"Using HTTPS certificate in ${https.keystoreReference.url}")
    bind(https.address, useHttps = true,
      newWebServiceActorRef(s"AgentWebService-https-${inetSocketAddressToName(https.address)}", authenticator),
      newServerSSLEngineProvider(https.keystoreReference))
  }

  private def newWebServiceActorRef(name: String, authenticator: UserPassAuthenticator[Account]) =
    actorSystem.actorOf(WebServiceActor.props(injector, authenticator), name)

  private def bind(address: InetSocketAddress, useHttps: Boolean, actorRef: ActorRef, sslEngineProvider: ServerSSLEngineProvider): Future[Unit] = {
    implicit val timeout: Timeout = 10.seconds
    val settings = ServerSettings(actorSystem).copy(sslEncryption = useHttps)
    (IO(Http) ? Http.Bind(actorRef, interface = address.getAddress.getHostAddress, port = address.getPort, settings = Some(settings))(sslEngineProvider))
      .map {
        case _: Http.Bound ⇒  // good
        case failed: Tcp.CommandFailed ⇒
          sys.error(s"Binding to TCP port $address failed: $failed. " +
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
  private val logger = Logger(getClass)

  private def newPortNeededException = new IllegalArgumentException("HTTP or HTTPS port is needed")

  private def inetSocketAddressToName(o: InetSocketAddress): String = o.getAddress.getHostAddress + s":${o.getPort}"
}
