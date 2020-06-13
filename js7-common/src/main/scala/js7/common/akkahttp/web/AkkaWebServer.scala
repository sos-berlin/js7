package js7.common.akkahttp.web

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.settings.{ParserSettings, ServerSettings}
import akka.http.scaladsl.{ConnectionContext, Http}
import akka.stream.{ActorMaterializer, TLSClientAuth}
import cats.effect.Resource
import cats.instances.vector._
import cats.syntax.traverse._
import com.typesafe.config.{Config, ConfigFactory}
import java.net.InetSocketAddress
import js7.base.generic.Completed
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils._
import js7.base.utils.ScalazStyle._
import js7.base.utils.{Lazy, SetOnce}
import js7.common.akkahttp.https.Https.loadSSLContext
import js7.common.akkahttp.web.AkkaWebServer._
import js7.common.akkahttp.web.data.WebServerBinding
import js7.common.http.JsonStreamingSupport
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.Logger
import js7.common.time.JavaTimeConverters.AsScalaDuration
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.Promise
import scala.util.control.NonFatal

/**
 * @author Joacim Zschimmer
 */
trait AkkaWebServer extends AutoCloseable
{
  protected implicit def actorSystem: ActorSystem
  protected def config: Config
  protected def bindings: Seq[WebServerBinding]
  protected def newRoute(binding: WebServerBinding): BoundRoute

  private val shuttingDownPromise = Promise[Completed]()
  protected final def isShuttingDown = shuttingDownPromise.future.isCompleted
  private lazy val akkaHttp = Http(actorSystem)
  private val materializerLazy = Lazy { ActorMaterializer() }
  private implicit def materializer = materializerLazy()
  private lazy val shutdownTimeout = config.getDuration("js7.webserver.shutdown-timeout").toFiniteDuration
  private val scheduler = SetOnce[Scheduler]

  private var activeBindings: Seq[Task[Http.ServerBinding]] = null

  /**
   * @return Future, completed when Agent has been started and is running.
   */
  def start(): Task[Completed] =
    Task.deferAction { scheduler =>
      this.scheduler := scheduler  // For close()
      if (bindings.isEmpty)
        Task.raiseError(new IllegalArgumentException("Web server needs a configured HTTP or HTTPS port"))
      else {
        logger.debug(bindings mkString ", ")
        activeBindings = bindings map {
          case o: WebServerBinding.Http => bindHttp(o).memoize
          case o: WebServerBinding.Https => bindHttps(o).memoize
        }
        Task.sequence(activeBindings).map(_ => Completed)
      }
    }

  private def bindHttp(http: WebServerBinding.Http): Task[Http.ServerBinding] =
    bind(http, akkaHttp.defaultServerHttpContext)

  private def bindHttps(https: WebServerBinding.Https): Task[Http.ServerBinding] = {
    logger.info(s"Using HTTPS certificate in ${https.keyStoreRef.url} for port ${https.toWebServerPort}")
    bind(
      https,
      ConnectionContext.https(
        loadSSLContext(Some(https.keyStoreRef), https.trustStoreRef),
        clientAuth = https.mutual ? TLSClientAuth.Need,
        sslConfig = None,
        enabledCipherSuites = None,
        enabledProtocols = None,
        sslParameters = None))
  }

  private def bind(binding: WebServerBinding, connectionContext: ConnectionContext): Task[Http.ServerBinding] = {
    val boundRoute = newRoute(binding)
    Task.deferFuture(
      akkaHttp.bindAndHandle(boundRoute.webServerRoute, interface = binding.address.getAddress.getHostAddress, port = binding.address.getPort,
        connectionContext,
        settings = ServerSettings(actorSystem)
          .withParserSettings(ParserSettings(actorSystem).withCustomMediaTypes(JsonStreamingSupport.CustomMediaTypes: _*)))
    ) .map { serverBinding =>
        logger.info(s"Bound ${binding.scheme}://${serverBinding.localAddress.getAddress.getHostAddress}:${serverBinding.localAddress.getPort}" +
          (if (binding.mutual) ", client certificate is required" else "") +
          boundRoute.boundMessageSuffix)
        serverBinding
      }
  }

  def close() =
    try terminate().runToFuture(scheduler.get) await shutdownTimeout + 1.s
    catch { case NonFatal(t) =>
      // In RecoveryTest: IllegalStateException: IO Listener actor terminated unexpectedly for remote endpoint [..]
      logger.debug(s"$toString close(): " + t.toStringWithCauses, t.nullIfNoStackTrace)
    }

  def terminate(): Task[Unit] =
    Task.defer {
      if (!shuttingDownPromise.trySuccess(Completed))
        Task.unit
      else if (activeBindings == null)
        Task.unit
      else
        Task { logger.debug("terminate") } >>
          Task.sleep(500.ms) >> // Wait a short time to let event streams finish, to avoid connection reset
          // Now wait until (event) Observables has been closed?
          activeBindings.toVector
            .traverse(_.flatMap(binding =>
              Task.deferFuture(binding.terminate(hardDeadline = shutdownTimeout))
                .map { _: Http.HttpTerminated =>
                  logger.debug(s"$binding terminated")
                  Completed
                }))
            .map((_: Seq[Completed]) => ())
    }.guarantee(Task {
      for (materializer <- materializerLazy if !materializer.isShutdown) {
        logger.debug("materializer.shutdown()")
        try materializer.shutdown()
        catch { case NonFatal(t) =>
          logger.warn(s"$toString close(): " + t.toStringWithCauses, t.nullIfNoStackTrace)
        }
      }
    })

  override def toString = s"${getClass.simpleScalaName}"
}

object AkkaWebServer
{
  private val logger = Logger(getClass)

  def http(port: Int, config: Config = ConfigFactory.empty)(route: Route)(implicit as: ActorSystem)
  : AkkaWebServer with HasUri = {
    new Standard(WebServerBinding.http(port) :: Nil, _ => BoundRoute(route), config) with AkkaWebServer.HasUri
  }

  class Standard(
    protected val bindings: Seq[WebServerBinding],
    route: WebServerBinding => BoundRoute,
    protected val config: Config = ConfigFactory.empty)
    (implicit protected val actorSystem: ActorSystem)
  extends AkkaWebServer
  {
    def newRoute(binding: WebServerBinding) = route(binding)
  }

  def resourceForHttp(
    httpPort: Int,
    route: Route,
    config: Config = ConfigFactory.empty)
    (implicit actorSystem: ActorSystem)
  : Resource[Task, AkkaWebServer with HasUri] =
    resource(WebServerBinding.http(httpPort) :: Nil, _ => BoundRoute(route), config)

  def resource(
    bindings: Seq[WebServerBinding],
    route: WebServerBinding => BoundRoute,
    config: Config = ConfigFactory.empty)
    (implicit actorSystem: ActorSystem)
  : Resource[Task, AkkaWebServer with HasUri] =
    Resource.make(
      Task.defer {
        val webServer = new Standard(bindings, route, config) with AkkaWebServer.HasUri
        webServer.start().map(_ => webServer)
      }
    )(_.terminate())

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
  object BoundRoute {
    def apply(route: Route): BoundRoute =
      new BoundRoute {
        def webServerRoute = route
      }
  }

  final class ForTest(protected val actorSystem: ActorSystem, route: Route) extends AkkaWebServer with AkkaWebServer.HasUri {
    protected val config = ConfigFactory.parseString("js7.webserver.shutdown-timeout = 10s")
    protected def scheduler = Scheduler.global
    protected lazy val bindings = WebServerBinding.Http(new InetSocketAddress("127.0.0.1", findFreeTcpPort())) :: Nil
    protected def newRoute(binding: WebServerBinding) = AkkaWebServer.BoundRoute(route)
  }
}
