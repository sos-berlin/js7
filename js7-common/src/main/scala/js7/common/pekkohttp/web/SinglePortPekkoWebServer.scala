package js7.common.pekkohttp.web

import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.catsutils.UnsafeMemoizable.given
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.server.Directives.extractRequest
import org.apache.pekko.http.scaladsl.server.Route
import org.apache.pekko.http.scaladsl.settings.{ParserSettings, ServerSettings}
import org.apache.pekko.http.scaladsl.{ConnectionContext, Http, HttpsConnectionContext}
import org.apache.pekko.stream.TLSClientAuth
import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import js7.base.io.https.Https.loadSSLContext
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.service.Service
import js7.base.utils.Atomic
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.typeclasses.IsEmpty.syntax.toIsEmptyAllOps
import js7.common.pekkohttp.web.PekkoWebServer.{BoundRoute, RouteBinding}
import js7.common.pekkohttp.web.SinglePortPekkoWebServer.*
import js7.common.pekkohttp.web.data.{WebServerBinding, WebServerPort}
import js7.common.http.JsonStreamingSupport
import js7.common.internet.IP.inetSocketAddressShow
import cats.effect.unsafe.IORuntime
import scala.concurrent.duration.{Deadline, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.chaining.scalaUtilChainingOps
import scala.util.{Failure, Success, Try}

/**
 * @author Joacim Zschimmer
 */
private final class SinglePortPekkoWebServer private(binding: Binding)
  (implicit protected val actorSystem: ActorSystem)
extends Service.StoppableByRequest:
  protected def start =
    startService(
      untilStopRequested *> onStop)

  private def onStop: IO[Unit] =
    binding.stop
      //.onErrorHandle(t =>
      //  logger.error(s"$toString $binding.terminate => ${t.toStringWithCauses}",
      //    t.nullIfNoStackTrace))

  override def toString = s"SinglePortPekkoWebServer($binding)"

private object SinglePortPekkoWebServer:
  private val logger = Logger[this.type]

  def resource(
    webServerBinding: WebServerBinding,
    toBoundRoute: RouteBinding => BoundRoute,
    shutdownTimeout: FiniteDuration,
    httpsClientAuthRequired: Boolean)
    (implicit actorSystem: ActorSystem)
  : Resource[IO, SinglePortPekkoWebServer] =
    Resource.suspend(IO {
      // The revision counter is saved in this memoized IO, see end of this io.
      val revision = Atomic(1)

      def makeBoundRoute(): (BoundRoute, Promise[Deadline]) = {
        val rev = revision.getAndIncrement()
        val terminatingPromise = Promise[Deadline]()
        toBoundRoute(RouteBinding(webServerBinding, rev, terminatingPromise.future)) ->
          terminatingPromise
      }

      resource2(webServerBinding, makeBoundRoute _, shutdownTimeout, httpsClientAuthRequired)
    }.unsafeMemoize/*saves the `revision` counter for multiple allocations*/)

  private def resource2(
    webServerBinding: WebServerBinding,
    makeBoundRoute: () => (BoundRoute, Promise[Deadline]),
    shutdownTimeout: FiniteDuration,
    httpsClientAuthRequired: Boolean)
    (implicit actorSystem: ActorSystem)
  : Resource[IO, SinglePortPekkoWebServer] =
    Service.resource(IO.defer {
      val pekkoHttp = Http(actorSystem)

      def bindHttps(https: WebServerBinding.Https): IO[Binding] = {
        logger.info(
          s"Using HTTPS certificate in ${https.keyStoreRef.url} for port ${https.toWebServerPort}")
        bind(
          https,
          Some(ConnectionContext.https(
            loadSSLContext(Some(https.keyStoreRef), https.trustStoreRefs),
            clientAuth = httpsClientAuthRequired ? TLSClientAuth.Need)))
      }

      def bind(
        binding: WebServerBinding,
        httpsConnectionContext: Option[HttpsConnectionContext] = None)
      : IO[Binding] =
        IO.defer {
          val serverBuilder = pekkoHttp
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

          val (boundRoute, terminatingPromise) = makeBoundRoute()
          val bindingString = s"${binding.scheme}://${binding.address.show}"

          for
            routeDelegator <- DelayedRouteDelegator.start(binding, boundRoute, bindingString)
            pekkoBinding <- IO.fromFutureWithEC(implicit ec => IO:
              val whenBound = serverBuilder.bind(routeDelegator.webServerRoute)
              terminatingPromise.completeWith(whenBound.flatMap(_.whenTerminationSignalIssued))
              whenBound)
          yield
            // An info line will be logged by DelayedRouteDelegator
            val securityHint = boundRoute.startupSecurityHint(binding.scheme)
            logger.debug(s"$bindingString is bound to $boundRoute$securityHint")
            Binding(binding, pekkoBinding, shutdownTimeout)
        }

      webServerBinding
        .match {
          case o: WebServerBinding.Http => bind(o)
          case o: WebServerBinding.Https => bindHttps(o)
        }
        .map(binding =>
          new SinglePortPekkoWebServer(binding))
    })

  /** Returns 503 ServiceUnavailable until the Route is provided. */
  private final class DelayedRouteDelegator(boundRoute: BoundRoute):
    private val _realRoute = Atomic(none[Try[Route]])

    def start(binding: WebServerBinding, name: String): IO[Unit] =
      boundRoute.webServerRoute
        .flatTap(realRoute => IO:
          if _realRoute.compareAndSet(None, Some(Success(realRoute))) then
            val serviceName = boundRoute.serviceName.emptyToNone.fold("")(_ + " ")
            val securityHint = boundRoute.startupSecurityHint(binding.scheme)
            logger.info(s"$name ${serviceName}web services are available$securityHint"))
        .start.void

    def webServerRoute: Route =
      extractRequest/*force recalculation with each call*/(_ =>
        selectBoundRoute)

    def selectBoundRoute: Route =
      _realRoute.get() match
        case None => boundRoute.stillNotAvailableRoute
        case Some(Failure(t)) => throw t
        case Some(Success(realRoute)) => realRoute

  private object DelayedRouteDelegator:
    def start(binding: WebServerBinding, boundRoute: BoundRoute, name: String)
    : IO[DelayedRouteDelegator] =
      IO.defer:
        val r = new DelayedRouteDelegator(boundRoute)
        r.start(binding, name).as(r)

  private final case class Binding(
    webServerBinding: WebServerBinding,
    pekkoBinding: Http.ServerBinding,
    shutdownTimeout: FiniteDuration):
    val webServerPort: WebServerPort =
      webServerBinding.toWebServerPort

    def stop: IO[Unit] =
      logger
        .debugIO(s"Terminate $toString"):
          IO.fromFuture(IO:
            pekkoBinding.terminate(hardDeadline = shutdownTimeout))
        .void

    override def toString = webServerPort.toString
