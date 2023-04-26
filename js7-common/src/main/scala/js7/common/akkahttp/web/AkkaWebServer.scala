package js7.common.akkahttp.web

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes.ServiceUnavailable
import akka.http.scaladsl.server.Directives.{complete, extractRequest}
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.settings.{ParserSettings, ServerSettings}
import akka.http.scaladsl.{ConnectionContext, Http, HttpsConnectionContext}
import akka.stream.TLSClientAuth
import cats.effect.Resource
import cats.syntax.all.*
import com.typesafe.config.{Config, ConfigFactory}
import js7.base.auth.SimpleUser
import js7.base.configutils.Configs.*
import js7.base.generic.Completed
import js7.base.io.https.Https.loadSSLContext
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.problem.Problem
import js7.base.service.Service
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.SetOnce
import js7.base.web.Uri
import js7.common.akkahttp.ExceptionHandling
import js7.common.akkahttp.StandardMarshallers.*
import js7.common.akkahttp.web.AkkaWebServer.*
import js7.common.akkahttp.web.auth.GateKeeper
import js7.common.akkahttp.web.data.{WebServerBinding, WebServerPort}
import js7.common.http.JsonStreamingSupport
import js7.common.internet.IP.inetSocketAddressShow
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.atomic.Atomic
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.Deadline
import scala.concurrent.{Future, Promise}
import scala.util.chaining.scalaUtilChainingOps
import scala.util.{Failure, Success}

/**
 * @author Joacim Zschimmer
 */
trait AkkaWebServer extends Service
{
  protected def serverBindings: Seq[Http.ServerBinding]
  protected def config: Config
  protected implicit def actorSystem: ActorSystem
  protected def scheduler: Scheduler

  private val shuttingDownPromise = Promise[Completed]()
  private val shutdownTimeout = config.finiteDuration("js7.web.server.shutdown-timeout").orThrow

  private val untilTerminated: Task[Unit] =
    serverBindings
      .traverse(o => Task
        .fromFuture(o.whenTerminated)
        .map((_: Http.HttpTerminated) => ()))
      .map(_.combineAll)
      .memoize

  protected final def start =
    startService(
      untilTerminated)

  protected val stop =
    terminate.*>(untilStopped).memoize

  private def terminate: Task[Unit] =
    Task.defer {
      if (!shuttingDownPromise.trySuccess(Completed))
        Task.unit
      else if (serverBindings == null)
        Task.unit
      else
        terminateBindings
    }

  private def terminateBindings: Task[Unit] =
    logger.debugTask(
      serverBindings.parTraverse(binding =>
        logger
          .debugTask(s"$toString terminate $binding")(
            Task.deferFuture(
              binding.terminate(hardDeadline = shutdownTimeout)))
          .void
          .onErrorHandle { t =>
            logger.error(s"$toString $binding.terminate => ${t.toStringWithCauses}",
              t.nullIfNoStackTrace)
          }
      ).void)

  override def toString = s"${getClass.shortClassName}"
}

object AkkaWebServer
{
  private val logger = Logger(getClass)
  private[web] val testConfig = config"""
    js7.web.server.auth.https-client-authentication = off
    js7.web.server.shutdown-timeout = 10s
    """

  @TestOnly
  def testResource()(route: Route)(implicit as: ActorSystem)
  : Resource[Task, AkkaWebServer & HasUri] =
    testUriAndResource()(route)._2

  @TestOnly
  def testUriAndResource()(route: Route)(implicit as: ActorSystem)
  : (Uri, Resource[Task, AkkaWebServer & HasUri]) =
    testResource(ConfigFactory.empty)(route)

  @TestOnly
  def testResource(config: Config)(route: Route)(implicit as: ActorSystem)
  : (Uri, Resource[Task, AkkaWebServer & HasUri]) =
    testResource(findFreeTcpPort(), config, route = route)

  @TestOnly
  def testResource(port: Int = findFreeTcpPort(), config: Config = ConfigFactory.empty, route: Route)(implicit as: ActorSystem)
  : (Uri, Resource[Task, AkkaWebServer & HasUri]) =
    Uri(s"http://127.0.0.1:$port") -> httpResource(port = port, config.withFallback(testConfig), route)

  def httpResource(port: Int, config: Config, route: Route)
    (implicit actorSystem: ActorSystem)
  : Resource[Task, AkkaWebServer & HasUri] =
    resource(
      Seq(WebServerBinding.http(port)),
      config,
      (_, whenTerminating) => Task.pure(BoundRoute(route, whenTerminating)))

  def resource(
    bindings: Seq[WebServerBinding],
    config: Config,
    toBoundRoute: (WebServerBinding, Future[Deadline]) => Task[BoundRoute])
    (implicit actorSystem: ActorSystem)
  : Resource[Task, AkkaWebServer & HasUri] =
    Service.resource(Task.defer {
      Task.deferAction { scheduler =>
        val httpsClientAuthRequired = config.getBoolean("js7.web.server.auth.https-client-authentication")
        val akkaHttp = Http(actorSystem)

        def bindHttps(https: WebServerBinding.Https): Task[Http.ServerBinding] = {
          logger.info(s"Using HTTPS certificate in ${https.keyStoreRef.url} for port ${https.toWebServerPort}")
          bind(
            https,
            Some(ConnectionContext.https(
              loadSSLContext(Some(https.keyStoreRef), https.trustStoreRefs),
              clientAuth = httpsClientAuthRequired ? TLSClientAuth.Need)))
        }

        def bind(
          binding: WebServerBinding,
          httpsConnectionContext: Option[HttpsConnectionContext] = None)
        : Task[Http.ServerBinding] =
          Task.deferAction { implicit scheduler =>
            val serverBuilder = akkaHttp
              .newServerAt(
                interface = binding.address.getAddress.getHostAddress,
                port = binding.address.getPort)
              .pipe(o => httpsConnectionContext.fold(o)(o.enableHttps))
              .withSettings(
                ServerSettings(actorSystem)
                  .withParserSettings(
                    ParserSettings(actorSystem)
                      .withCustomMediaTypes(JsonStreamingSupport.CustomMediaTypes *)
                      .withMaxContentLength(JsonStreamingSupport.JsonObjectMaxSize /*js7.conf ???*/)))

            val terminatingPromise = Promise[Deadline]()
            val whenTerminating = terminatingPromise.future
            val boundRoute = new DelegatingBoundRoute(
              binding.scheme, toBoundRoute(binding, whenTerminating), config)
            for {
              serverBinding <-
                Task.deferFutureAction { implicit scheduler =>
                  val whenBound = serverBuilder.bind(boundRoute.webServerRoute)
                  terminatingPromise.completeWith(whenBound.flatMap(_.whenTerminationSignalIssued))
                  whenBound
                }
            } yield {
              logger.info(
                s"Bound ${binding.scheme}://${serverBinding.localAddress.show}${boundRoute.boundMessageSuffix}")
              serverBinding
            }
          }

        bindings
          .traverse {
            case o: WebServerBinding.Http => bind(o)
            case o: WebServerBinding.Https => bindHttps(o)
          }.map(serverBindings =>
            new Standard(
              serverBindings,
              webServerPorts = bindings.map(_.toWebServerPort).toList,
              config
            )(actorSystem, scheduler))
      }
    })

  final class Standard(
    protected val serverBindings: Seq[Http.ServerBinding],
    protected val webServerPorts: Seq[WebServerPort],
    protected val config: Config)
    (implicit
      protected val actorSystem: ActorSystem,
      protected val scheduler: Scheduler)
    extends AkkaWebServer with HasUri

  trait HasUri extends WebServerBinding.HasLocalUris {
    this: AkkaWebServer =>
  }

  def actorName(prefix: String, binding: WebServerBinding) = {
    import binding.{address, scheme}
    s"$prefix-$scheme-${address.getAddress.getHostAddress}:${address.getPort}"
  }

  trait BoundRoute {
    def webServerRoute: Route
    /** Suffix for the bound log message, for example a security hint. */
    def boundMessageSuffix: String
  }
  object BoundRoute {
    def apply(route: Route, whenTerminating: Future[Deadline]): BoundRoute =
      new BoundRoute {
        def webServerRoute = route

        def boundMessageSuffix = ""
      }
  }

  private final class DelegatingBoundRoute(
    scheme: WebServerBinding.Scheme,
    realBoundRoute: Task[BoundRoute],
    protected val config: Config)
    (implicit scheduler: Scheduler)
  extends BoundRoute {
    private val whenRealBound = realBoundRoute.runToFuture
    private val _boundRoute = Atomic(none[BoundRoute])

    private object stillNotAvailable extends BoundRoute {
      val webServerRoute =
        complete(ServiceUnavailable -> Problem("Still starting"))

      lazy val boundMessageSuffix =
        GateKeeper.Configuration.fromConfig(config, SimpleUser.apply, Nil)
          .secureStateString(scheme)
    }

    def webServerRoute =
      extractRequest/*force recalculation with each call*/(_ =>
        selectBoundRoute.webServerRoute)

    def boundMessageSuffix =
      selectBoundRoute.boundMessageSuffix

    def selectBoundRoute: BoundRoute =
      _boundRoute.get().getOrElse(
        whenRealBound.value match {
          case None => stillNotAvailable
          case Some(Failure(t)) => throw t
          case Some(Success(realBoundRoute)) =>
            if (!_boundRoute.compareAndSet(None, Some(realBoundRoute))) {
              logger.debug(s"$realBoundRoute is ready")
            }
            realBoundRoute
        })
  }
}
