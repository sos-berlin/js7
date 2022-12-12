package js7.common.akkahttp.web

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.settings.{ParserSettings, ServerSettings}
import akka.http.scaladsl.{ConnectionContext, Http, HttpsConnectionContext}
import akka.stream.TLSClientAuth
import cats.effect.Resource
import cats.effect.concurrent.Deferred
import cats.syntax.foldable.*
import cats.syntax.show.*
import cats.syntax.traverse.*
import com.typesafe.config.{Config, ConfigFactory}
import js7.base.configutils.Configs.*
import js7.base.generic.Completed
import js7.base.io.https.Https.loadSSLContext
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.service.Service
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.Uri
import js7.common.akkahttp.web.AkkaWebServer.*
import js7.common.akkahttp.web.data.{WebServerBinding, WebServerPort}
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
trait AkkaWebServer extends Service
{
  protected def serverBindings: Seq[Http.ServerBinding]
  protected def config: Config
  protected implicit def actorSystem: ActorSystem
  protected def scheduler: Scheduler

  private val shuttingDownPromise = Promise[Completed]()
  private val shutdownTimeout = config.finiteDuration("js7.web.server.shutdown-timeout").orThrow
  private val stopTimedOut = Deferred.unsafe[Task, Unit]

  private val untilTerminated: Task[Unit] =
    serverBindings
      .traverse(o => Task
        .fromFuture(o.whenTerminated)
        .map((_: Http.HttpTerminated) => ()))
      .map(_.combineAll)
      .memoize

  protected final def start =
    startService(
      Task.racePair(untilTerminated, stopTimedOut.get).void)

  def stop = {
    val longerTimeout = shutdownTimeout + 2.s
    stop(shutdownTimeout)
      .timeoutTo(
        longerTimeout,
        Task.defer {
          logger.debug(s"Cancelling AkkaWebServer.stop after ${longerTimeout.pretty}")
          stopTimedOut.complete(())
        })
  }

  def stop(timeout: FiniteDuration = shutdownTimeout): Task[Unit] =
    terminate(timeout)
      .executeOn(scheduler)
      .uncancelable
      .timeout(timeout + 1.s)
      .onErrorHandle {
        case NonFatal(t: TimeoutException) =>
          logger.warn(s"$toString while shuttig down the web server: " + t.toStringWithCauses)
        case NonFatal(t) =>
          logger.warn(s"$toString close(): " + t.toStringWithCauses, t.nullIfNoStackTrace)
      }

  private def terminate(timeout: FiniteDuration): Task[Unit] =
    Task.defer {
      if (!shuttingDownPromise.trySuccess(Completed))
        Task.unit
      else if (serverBindings == null)
        Task.unit
      else
        terminateBindings(timeout)
    }

  private def terminateBindings(timeout: FiniteDuration): Task[Unit] =
    logger.debugTask(
      serverBindings.traverse(binding =>
        Task.deferFuture(binding.terminate(hardDeadline = timeout))
          .map { (_: Http.HttpTerminated) =>
            logger.debug(s"$binding terminated")
            Completed
          })
        .map((_: Seq[Completed]) => ()))

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
    route: (WebServerBinding, Future[Deadline]) => Task[BoundRoute])
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

            val whenTerminating = Promise[Deadline]()
            for {
              boundRoute <- route(binding, whenTerminating.future)
              serverBinding <-
                Task.deferFutureAction { implicit s =>
                  val whenBound = serverBuilder.bind(boundRoute.webServerRoute)
                  whenTerminating.completeWith(whenBound.flatMap(_.whenTerminationSignalIssued))
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
    def boundMessageSuffix = ""
  }
  object BoundRoute {
    def apply(route: Route, whenTerminating: Future[Deadline]): BoundRoute =
      new BoundRoute {
        def webServerRoute = route
      }
  }
}
