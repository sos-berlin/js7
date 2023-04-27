package js7.common.akkahttp.web

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives.{complete, extractRequest}
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.settings.{ParserSettings, ServerSettings}
import akka.http.scaladsl.{ConnectionContext, Http, HttpsConnectionContext}
import akka.stream.TLSClientAuth
import cats.effect.Resource
import cats.syntax.all.*
import com.typesafe.config.{Config, ConfigFactory}
import js7.base.configutils.Configs.*
import js7.base.generic.Completed
import js7.base.io.https.Https.loadSSLContext
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.problem.Problems.WebServiceStillNotAvailableProblem
import js7.base.service.Service
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.Uri
import js7.common.akkahttp.StandardMarshallers.*
import js7.common.akkahttp.web.AkkaWebServer.*
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
    serverBindings
      .parTraverse(binding =>
        logger
          .debugTask(s"$toString terminate $binding")(
            Task.deferFuture(
              binding.terminate(hardDeadline = shutdownTimeout)))
          .void
          .onErrorHandle { t =>
            logger.error(s"$toString $binding.terminate => ${t.toStringWithCauses}",
              t.nullIfNoStackTrace)
          })
      .map(_.combineAll)

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
      (_, _) => BoundRoute.simple(route))

  def resource(
    bindings: Seq[WebServerBinding],
    config: Config,
    toBoundRoute: (WebServerBinding, Future[Deadline]) => BoundRoute)
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
                      .withCustomMediaTypes(JsonStreamingSupport.CustomMediaTypes *)
                      .withMaxContentLength(JsonStreamingSupport.JsonObjectMaxSize /*js7.conf ???*/)))

            val terminatingPromise = Promise[Deadline]()
            val whenTerminating = terminatingPromise.future
            val boundRoute = toBoundRoute(binding, whenTerminating)
            val name = s"${binding.scheme}://${binding.address.show}"
            for {
              serverBinding <-
                Task.deferFutureAction { implicit scheduler =>
                  val routeDelegator = new DelayedRouteDelegator(boundRoute, name)
                  val whenBound = serverBuilder.bind(routeDelegator.webServerRoute)
                  terminatingPromise.completeWith(whenBound.flatMap(_.whenTerminationSignalIssued))
                  whenBound
                }
            } yield {
              val securityHint = boundRoute.startupSecurityHint(binding.scheme)
              logger.debug(s"$name is bound$securityHint")
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

  private lazy val stillNotAvailableRoute: Route =
    complete(WebServiceStillNotAvailableProblem)

  trait BoundRoute {
    def stillNotAvailableRoute: Route =
      AkkaWebServer.stillNotAvailableRoute

    def webServerRoute: Task[Route]

    def startupSecurityHint(scheme: WebServerBinding.Scheme): String
  }
  object BoundRoute {
    def simple(route: Route): BoundRoute =
      new BoundRoute {
        def webServerRoute = Task.pure(route)
        def startupSecurityHint(scheme: WebServerBinding.Scheme) = ""
      }
  }

  /** Returns 503 ServiceUnavailable until the Route is provided. */
  private final class DelayedRouteDelegator(boundRoute: BoundRoute, name: String)
    (implicit scheduler: Scheduler)
  {
    private val _realRoute = Atomic(none[Route])

    private val whenRealRoute = boundRoute.webServerRoute
      .tapEval(realRoute => Task {
        if (_realRoute.compareAndSet(None, Some(realRoute))) {
          logger.info(s"$name web services are available")
        }
      })
      .runToFuture

    def webServerRoute: Route =
      extractRequest/*force recalculation with each call*/(_ =>
        selectBoundRoute)

    def selectBoundRoute: Route =
      _realRoute.get().getOrElse(
        whenRealRoute.value match {
          case None => boundRoute.stillNotAvailableRoute
          case Some(Failure(t)) => throw t
          case Some(Success(realRoute)) => realRoute
        })
  }
}
