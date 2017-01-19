package com.sos.scheduler.engine.common.sprayutils.web

import akka.actor.{ActorRef, ActorSystem}
import akka.io.{IO, Tcp}
import akka.pattern.ask
import akka.util.Timeout
import com.sos.scheduler.engine.base.utils.ScalaUtils.RichAny
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.sprayutils.WebServerBinding
import com.sos.scheduler.engine.common.sprayutils.https.Https.newServerSSLEngineProvider
import com.sos.scheduler.engine.common.sprayutils.web.SprayWebServer._
import com.sos.scheduler.engine.common.time.ScalaTime._
import java.net.InetSocketAddress
import java.util.concurrent.ConcurrentLinkedQueue
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Success
import spray.can.Http
import spray.can.Http.Unbind
import spray.can.server.ServerSettings
import spray.http.Uri
import spray.io.ServerSSLEngineProvider

/**
 * @author Joacim Zschimmer
 */
trait SprayWebServer extends AutoCloseable {

  protected implicit def actorSystem: ActorSystem
  protected implicit def executionContext: ExecutionContext
  protected def newRouteActorRef(binding: WebServerBinding): ActorRef
  protected def bindings: Iterable[WebServerBinding]
  private val httpListeners = new ConcurrentLinkedQueue[ActorRef]

  /**
   * @return Future, completed when Agent has been started and is running.
   */
  def start(): Future[Unit] =
    if (bindings.isEmpty)
      Future.failed(newPortNeededException)
    else {
      logger.debug(s"Binding to ${bindings mkString ", "}")
      val bound: Iterable[Future[Unit]] = bindings map {
        case o: WebServerBinding.Http ⇒ bindHttp(o)
        case o: WebServerBinding.Https ⇒ bindHttps(o)
      }
      Future.sequence(bound) map { _ ⇒ () }
    }

  private def bindHttp(http: WebServerBinding.Http): Future[Unit] =
    bind(http.address, useHttps = false,
      newRouteActorRef(http),
      implicitly[ServerSSLEngineProvider])

  private def bindHttps(https: WebServerBinding.Https): Future[Unit] = {
    logger.info(s"Using HTTPS certificate in ${https.keystoreReference.url}")
    bind(https.address, useHttps = true,
      newRouteActorRef(https),
      newServerSSLEngineProvider(https.keystoreReference))
  }

  private def bind(address: InetSocketAddress, useHttps: Boolean, actorRef: ActorRef, sslEngineProvider: ServerSSLEngineProvider): Future[Unit] = {
    implicit val timeout: Timeout = 10.seconds
    val settings = ServerSettings(actorSystem).copy(sslEncryption = useHttps)
    (IO(Http) ? Http.Bind(actorRef, interface = address.getAddress.getHostAddress, port = address.getPort, settings = Some(settings))(sslEngineProvider))
      .map {
        case _: Http.Bound ⇒  // good
        case failed: Tcp.CommandFailed ⇒
          sys.error(s"Binding to TCP port $address failed. " +
            "Port is possibly in use and not available. " +
            s"Switch on DEBUG-level logging for `akka.io.TcpListener` to log the cause. $failed")
            // (Akka 2.3.7) When Akka #13861 should be fixed, replace by actual exception. See https://github.com/akka/akka/issues/13861
    }
  }

  def close() = {
    // TODO Send Http.Unbind to senders of Http.Bound (we need an Actor to get the sender()).
  }
}

object SprayWebServer {
  private val ShutdownTimeout = 5.s
  private val logger = Logger(getClass)

  trait HasUri {
    this: SprayWebServer ⇒
    protected def uriPathPrefix: String

    lazy val localHttpUriOption: Option[Uri] = bindings collectFirst { case o: WebServerBinding.Http ⇒ toLocallyUsableUri("http", o.address) }
    lazy val localHttpsUriOption: Option[Uri] = bindings collectFirst { case o: WebServerBinding.Https ⇒ toLocallyUsableUri("https", o.address) }

    lazy val localUri: Uri = localHttpUriOption orElse localHttpsUriOption getOrElse { throw newPortNeededException }

    private def toLocallyUsableUri(scheme: String, address: InetSocketAddress) = {
      val host = address.getAddress.getHostAddress.substitute("0.0.0.0", "127.0.0.1")
      val port = address.getPort
      Uri(s"$scheme://$host:$port/$uriPathPrefix" stripSuffix "/")
    }
  }

  def actorName(prefix: String, binding: WebServerBinding) =
    s"$prefix-${binding.scheme}-${inetSocketAddressToName(binding.address)}"

  private def inetSocketAddressToName(o: InetSocketAddress): String = o.getAddress.getHostAddress + s":${o.getPort}"

  private def newPortNeededException = new IllegalArgumentException("HTTP or HTTPS port is needed")
}
