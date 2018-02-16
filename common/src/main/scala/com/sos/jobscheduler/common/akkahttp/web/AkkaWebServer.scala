package com.sos.jobscheduler.common.akkahttp.web

import akka.actor.ActorSystem
import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.{ConnectionContext, Http}
import akka.stream.ActorMaterializer
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.utils.ScalaUtils.RichAny
import com.sos.jobscheduler.common.akkahttp.WebServerBinding
import com.sos.jobscheduler.common.akkahttp.https.Https.toHttpsConnectionContext
import com.sos.jobscheduler.common.akkahttp.web.AkkaWebServer._
import com.sos.jobscheduler.common.scalautil.Futures.implicits.RichFutures
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import java.net.InetSocketAddress
import scala.collection.immutable.Seq
import scala.concurrent.{ExecutionContext, Future}

/**
 * @author Joacim Zschimmer
 */
trait AkkaWebServer extends AutoCloseable {

  protected implicit def actorSystem: ActorSystem
  protected implicit def executionContext: ExecutionContext
  protected def newRoute(binding: WebServerBinding): Route
  protected def bindings: Seq[WebServerBinding]
  private val akkaHttp = Http(actorSystem)
  private implicit val materializer = ActorMaterializer()

  private var activeBindings: Seq[Future[Http.ServerBinding]] = null

  /**
   * @return Future, completed when Agent has been started and is running.
   */
  def start(): Future[Completed] =
    if (bindings.isEmpty)
      Future.failed(newPortNeededException)
    else {
      logger.debug(s"Binding to ${bindings mkString ", "}")
      activeBindings = bindings map {
        case o: WebServerBinding.Http ⇒ bindHttp(o)
        case o: WebServerBinding.Https ⇒ bindHttps(o)
      }
      Future.sequence(activeBindings) map { _ ⇒ Completed }
    }

  private def bindHttp(http: WebServerBinding.Http): Future[Http.ServerBinding] =
    bind(http, akkaHttp.defaultServerHttpContext)

  private def bindHttps(https: WebServerBinding.Https): Future[Http.ServerBinding] = {
    logger.info(s"Using HTTPS certificate in ${https.keystoreReference.url}")
    bind(https, toHttpsConnectionContext(https.keystoreReference))
  }

  private def bind(binding: WebServerBinding, connectionContext: ConnectionContext): Future[Http.ServerBinding] =
    akkaHttp.bindAndHandle(newRoute(binding), interface = binding.address.getAddress.getHostAddress, port = binding.address.getPort,
      connectionContext)

  def close() = {
    materializer.shutdown()
    if (activeBindings != null) {
      (for (future ← activeBindings) yield
        for (binding ← future) yield
          binding.unbind()
      ) await ShutdownTimeout
      //akkaHttp.gracefulShutdown()  https://github.com/akka/akka-http/issues/188, https://github.com/lagom/lagom/issues/644
    }
  }
}

object AkkaWebServer {
  private val ShutdownTimeout = 10.s
  private val logger = Logger(getClass)

  trait HasUri {
    this: AkkaWebServer ⇒
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
