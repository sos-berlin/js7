package com.sos.jobscheduler.common.akkahttp.web

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.settings.{ParserSettings, ServerSettings}
import akka.http.scaladsl.{ConnectionContext, Http}
import akka.stream.{ActorMaterializer, TLSClientAuth}
import cats.instances.future._
import cats.instances.vector._
import cats.syntax.traverse._
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.ScalaUtils._
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.akkahttp.JsonStreamingSupport.`application/json-seq`
import com.sos.jobscheduler.common.akkahttp.https.Https.loadSSLContext
import com.sos.jobscheduler.common.akkahttp.web.AkkaWebServer._
import com.sos.jobscheduler.common.akkahttp.web.data.WebServerBinding
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.JavaTimeConverters.AsScalaDuration
import com.typesafe.config.Config
import java.net.InetSocketAddress
import monix.execution.Scheduler
import scala.collection.immutable.Seq
import scala.concurrent.Future
import scala.util.control.NonFatal

/**
 * @author Joacim Zschimmer
 */
trait AkkaWebServer extends AutoCloseable
{
  protected implicit def actorSystem: ActorSystem
  protected def config: Config
  protected def scheduler: Scheduler
  protected def bindings: Seq[WebServerBinding]
  protected def newRoute(binding: WebServerBinding): BoundRoute

  private val akkaHttp = Http(actorSystem)
  private implicit val materializer = ActorMaterializer()
  private lazy val shutdownTimeout = config.getDuration("jobscheduler.webserver.shutdown-timeout").toFiniteDuration

  private var activeBindings: Seq[Future[Http.ServerBinding]] = null

  private implicit def implicitScheduler: Scheduler = scheduler

  /**
   * @return Future, completed when Agent has been started and is running.
   */
  def start(): Future[Completed] =
    if (bindings.isEmpty)
      Future.failed(new IllegalArgumentException("Web server needs a configured HTTP or HTTPS port"))
    else {
      logger.debug(bindings mkString ", ")
      activeBindings = bindings map {
        case o: WebServerBinding.Http => bindHttp(o)
        case o: WebServerBinding.Https => bindHttps(o)
      }
      Future.sequence(activeBindings) map { _ => Completed }
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

  private def bind(binding: WebServerBinding, connectionContext: ConnectionContext): Future[Http.ServerBinding] = {
    val boundRoute = newRoute(binding)
    akkaHttp.bindAndHandle(boundRoute.webServerRoute, interface = binding.address.getAddress.getHostAddress, port = binding.address.getPort,
      connectionContext,
      settings = ServerSettings(actorSystem)
        .withParserSettings(ParserSettings(actorSystem) withCustomMediaTypes `application/json-seq`))
    .map { serverBinding =>
      logger.info(s"Bound ${binding.scheme}://${serverBinding.localAddress.getAddress.getHostAddress}:${serverBinding.localAddress.getPort}" +
        (if (binding.mutual) ", client certificate is required" else "") +
        boundRoute.boundMessageSuffix)
      serverBinding
    }
  }

  def close() =
    try terminate() await shutdownTimeout + 1.s
    catch { case NonFatal(t) =>
      // In RecoveryTest: IllegalStateException: IO Listener actor terminated unexpectedly for remote endpoint [..]
      logger.debug(t.toStringWithCauses, t)
    }

  private def terminate(): Future[Completed.type] = {
    materializer.shutdown()
    if (activeBindings == null)
      Future.successful(Completed)
    else
      activeBindings.toVector.traverse(_.flatMap(binding =>
        binding.terminate(hardDeadline = shutdownTimeout)
          .map { _: Http.HttpTerminated =>
            logger.debug(s"$binding terminated")
            Completed
          }))
        .map((_: Seq[Completed]) => Completed)
  }
}

object AkkaWebServer {
  private val logger = Logger(getClass)

  trait HasUri extends WebServerBinding.HasLocalUris {
    this: AkkaWebServer =>

    protected final def webServerPorts = bindings map (_.toWebServerPort)
  }

  def actorName(prefix: String, binding: WebServerBinding) =
    s"$prefix-${binding.scheme}-${inetSocketAddressToName(binding.address)}"

  private def inetSocketAddressToName(o: InetSocketAddress): String = o.getAddress.getHostAddress + s":${o.getPort}"

  trait BoundRoute {
    def webServerRoute: Route
    /** Suffix for the bound log message, for example a security hint. */
    def boundMessageSuffix = ""
  }
}
