package js7.common.akkahttp.web

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.settings.{ParserSettings, ServerSettings}
import akka.http.scaladsl.{ConnectionContext, Http, HttpsConnectionContext}
import akka.stream.TLSClientAuth
import cats.effect.Resource
import cats.instances.vector._
import cats.syntax.show._
import cats.syntax.traverse._
import com.typesafe.config.{Config, ConfigFactory}
import js7.base.configutils.Configs._
import js7.base.generic.Completed
import js7.base.io.https.Https.loadSSLContext
import js7.base.log.Logger
import js7.base.thread.Futures.implicits._
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.SetOnce
import js7.common.akkahttp.web.AkkaWebServer._
import js7.common.akkahttp.web.data.WebServerBinding
import js7.common.http.JsonStreamingSupport
import js7.common.internet.IP.inetSocketAddressShow
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import monix.eval.Task
import monix.execution.Scheduler
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.{Deadline, FiniteDuration}
import scala.concurrent.{Future, Promise, TimeoutException}
import scala.util.chaining.scalaUtilChainingOps
import scala.util.control.NonFatal

/**
 * @author Joacim Zschimmer
 */
trait AkkaWebServer extends AutoCloseable
{
  protected implicit def actorSystem: ActorSystem
  protected def config: Config
  protected def bindings: Seq[WebServerBinding]
  protected def newBoundRoute(binding: WebServerBinding, whenTerminating: Future[Deadline])
  : Task[BoundRoute]

  private val shuttingDownPromise = Promise[Completed]()
  protected final def isShuttingDown = shuttingDownPromise.future.isCompleted
  private lazy val akkaHttp = Http(actorSystem)
  private lazy val shutdownTimeout = config.getDuration("js7.web.server.shutdown-timeout").toFiniteDuration
  private lazy val httpsClientAuthRequired = config.getBoolean("js7.web.server.auth.https-client-authentication")
  private val scheduler = SetOnce[Scheduler]

  private var activeBindings: Seq[Task[Http.ServerBinding]] = null

  /**
   * @return Future, completed when Agent has been started and is running.
   */
  def start: Task[Completed] =
    Task.deferAction { scheduler =>
      shutdownTimeout
      this.scheduler := scheduler  // For close()
      if (bindings.isEmpty)
        Task.raiseError(new IllegalArgumentException("Web server needs a configured HTTP or HTTPS port"))
      else {
        activeBindings = bindings map {
          case o: WebServerBinding.Http => bindHttp(o).memoize
          case o: WebServerBinding.Https => bindHttps(o).memoize
        }
        Task.sequence(activeBindings).map(_ => Completed)
      }
    }

  private def bindHttp(http: WebServerBinding.Http): Task[Http.ServerBinding] =
    bind(http)

  private def bindHttps(https: WebServerBinding.Https): Task[Http.ServerBinding] = {
    logger.info(s"Using HTTPS certificate in ${https.keyStoreRef.url} for port ${https.toWebServerPort}")
    bind(
      https,
      Some(ConnectionContext.https(
        loadSSLContext(Some(https.keyStoreRef), https.trustStoreRefs),
        clientAuth = httpsClientAuthRequired ? TLSClientAuth.Need)))
  }

  private def bind(
    binding: WebServerBinding,
    httpsConnectionContext: Option[HttpsConnectionContext] = None)
  : Task[Http.ServerBinding] = {
    Task.defer {
      val serverBuilder = akkaHttp
        .newServerAt(
          interface = binding.address.getAddress.getHostAddress,
          port = binding.address.getPort)
        .pipe(o => httpsConnectionContext.fold(o)(o.enableHttps))
        .withSettings(
          ServerSettings(actorSystem)
            .withParserSettings(
              ParserSettings(actorSystem)
                .withCustomMediaTypes(JsonStreamingSupport.CustomMediaTypes: _*)
                .withMaxContentLength(JsonStreamingSupport.JsonObjectMaxSize/*js7.conf ???*/)))

      val whenTerminating = Promise[Deadline]()
      for {
        boundRoute <- newBoundRoute(binding, whenTerminating.future)
        serverBinding <-
          Task.deferFutureAction { implicit s =>
            val whenBound = serverBuilder.bind(boundRoute.webServerRoute)
            whenTerminating.completeWith(whenBound.flatMap(_.whenTerminationSignalIssued))
            whenBound
          }
      } yield {
        logger.info(s"Bound ${ binding.scheme }://${ serverBinding.localAddress.show }" +
          boundRoute.boundMessageSuffix)
        serverBinding
      }
    }
  }

  def close() =
    for (scheduler <- scheduler) {
      stop(shutdownTimeout)
        .runToFuture(scheduler)
        .await(shutdownTimeout + 2.s)
    }

  def stop(timeout: FiniteDuration = shutdownTimeout): Task[Unit] =
    Task.defer {
      scheduler.toOption.fold(Task.unit)(scheduler =>
        terminate(timeout)
          .executeOn(scheduler)
          .uncancelable
          .timeout(timeout + 1.s)
          .onErrorHandle {
            case NonFatal(t: TimeoutException) =>
              logger.warn(s"$toString while shuttig down the web server: " + t.toStringWithCauses)
            case NonFatal(t) =>
              logger.warn(s"$toString close(): " + t.toStringWithCauses, t.nullIfNoStackTrace)
          })
    }

  def terminate(timeout: FiniteDuration): Task[Unit] =
    Task.defer {
      if (!shuttingDownPromise.trySuccess(Completed))
        Task.unit
      else if (activeBindings == null)
        Task.unit
      else
        Task { logger.debug("terminate") } >>
          terminateBindings(timeout)
    }

  private def terminateBindings(timeout: FiniteDuration): Task[Unit] =
    activeBindings.toVector
      .traverse(_.flatMap(binding =>
        Task.deferFuture(binding.terminate(hardDeadline = timeout))
          .map { _: Http.HttpTerminated =>
            logger.debug(s"$binding terminated")
            Completed
          }))
      .map((_: Seq[Completed]) => ())

  override def toString = s"${getClass.simpleScalaName}"
}

object AkkaWebServer
{
  private val logger = Logger(getClass)
  private[web] val testConfig = config"""
    js7.web.server.auth.https-client-authentication = off
    js7.web.server.shutdown-timeout = 10s
    """

  @TestOnly
  def forTest()(route: Route)(implicit as: ActorSystem): AkkaWebServer with HasUri =
    forTest(ConfigFactory.empty)(route)

  @TestOnly
  def forTest(config: Config)(route: Route)(implicit as: ActorSystem): AkkaWebServer with HasUri =
    forTest(findFreeTcpPort(), config, route = route)

  @TestOnly
  def forTest(port: Int = findFreeTcpPort(), config: Config = ConfigFactory.empty, route: Route)(implicit as: ActorSystem)
  : AkkaWebServer with HasUri =
    http(port = port, config, route)

  def http(port: Int = findFreeTcpPort(), config: Config = testConfig, route: Route)(implicit as: ActorSystem)
  : AkkaWebServer with HasUri =
    new Standard(
      WebServerBinding.http(port) :: Nil,
      config.withFallback(testConfig),
      (_, whenTerminating) => Task.pure(BoundRoute(route, whenTerminating)))

  class Standard(
    protected val bindings: Seq[WebServerBinding],
    protected val config: Config,
    route: (WebServerBinding, Future[Deadline]) => Task[BoundRoute])
    (implicit protected val actorSystem: ActorSystem)
  extends AkkaWebServer with HasUri
  {
    def newBoundRoute(binding: WebServerBinding, whenTerminating: Future[Deadline]) =
      route(binding, whenTerminating)
  }

  def resourceForHttp(
    httpPort: Int,
    route: Route,
    config: Config)
    (implicit actorSystem: ActorSystem)
  : Resource[Task, AkkaWebServer with HasUri] =
    resource(
      WebServerBinding.http(httpPort) :: Nil,
      config,
      (_, whenTerminating) => Task.pure(BoundRoute(route, whenTerminating)))

  def resource(
    bindings: Seq[WebServerBinding],
    config: Config,
    route: (WebServerBinding, Future[Deadline]) => Task[BoundRoute])
    (implicit actorSystem: ActorSystem)
  : Resource[Task, AkkaWebServer with HasUri] =
    Resource.make(
      acquire =
        Task.defer {
          val webServer = new Standard(bindings, config, route)
          webServer.start.map(_ => webServer)
        })(
      release = _.stop())

  trait HasUri extends WebServerBinding.HasLocalUris {
    this: AkkaWebServer =>

    protected final def webServerPorts = bindings.map(_.toWebServerPort)
  }

  def actorName(prefix: String, binding: WebServerBinding) = {
    import binding.{address, scheme}
    s"$prefix-$scheme-${address.getAddress.getHostAddress}:${address.getPort}"
  }

  trait BoundRoute {
    def webServerRoute: Route
    /** Suffix for the bound log message, for example a security hint. */
    def boundMessageSuffix = ""
  }
  object BoundRoute {
    def apply(route: Route, whenTerminating: Future[Deadline]): BoundRoute =
      new BoundRoute {
        def webServerRoute = route
      }
  }
}
