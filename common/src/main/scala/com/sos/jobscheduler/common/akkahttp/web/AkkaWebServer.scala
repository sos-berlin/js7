package com.sos.jobscheduler.common.akkahttp.web

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.settings.{ParserSettings, ServerSettings}
import akka.http.scaladsl.{ConnectionContext, Http}
import akka.stream.{ActorMaterializer, TLSClientAuth}
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.akkahttp.https.Https.loadSSLContext
import com.sos.jobscheduler.common.akkahttp.web.AkkaWebServer._
import com.sos.jobscheduler.common.akkahttp.web.data.WebServerBinding
import com.sos.jobscheduler.common.http.CirceJsonSeqSupport.`application/json-seq`
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
      Future.failed(new IllegalArgumentException("Web server needs a configured HTTP or HTTPS port"))
    else {
      logger.debug(bindings mkString ", ")
      activeBindings = bindings map {
        case o: WebServerBinding.Http ⇒ bindHttp(o)
        case o: WebServerBinding.Https ⇒ bindHttps(o)
      }
      Future.sequence(activeBindings) map { _ ⇒ Completed }
    }

  private def bindHttp(http: WebServerBinding.Http): Future[Http.ServerBinding] =
    bind(http, akkaHttp.defaultServerHttpContext)

  private def bindHttps(https: WebServerBinding.Https): Future[Http.ServerBinding] = {
    logger.info(s"Using HTTPS certificate in ${https.keyStoreRef.url} for ${https.toWebServerPort}")
    bind(
      https,
      ConnectionContext.https(
        loadSSLContext(Some(https.keyStoreRef), https.trustStoreRef),
        clientAuth = https.mutual ? TLSClientAuth.Need))
  }

  private def bind(binding: WebServerBinding, connectionContext: ConnectionContext): Future[Http.ServerBinding] =
    akkaHttp.bindAndHandle(newRoute(binding), interface = binding.address.getAddress.getHostAddress, port = binding.address.getPort,
      connectionContext,
      settings = ServerSettings(actorSystem)
        .withParserSettings(ParserSettings(actorSystem) withCustomMediaTypes `application/json-seq`))

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

  trait HasUri extends WebServerBinding.HasLocalUris {
    this: AkkaWebServer ⇒

    protected final def webServerPorts = bindings map (_.toWebServerPort)
  }

  def actorName(prefix: String, binding: WebServerBinding) =
    s"$prefix-${binding.scheme}-${inetSocketAddressToName(binding.address)}"

  private def inetSocketAddressToName(o: InetSocketAddress): String = o.getAddress.getHostAddress + s":${o.getPort}"
}
