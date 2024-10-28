package js7.common.pekkohttp.web

import cats.effect.{Deferred, IO, Resource, ResourceIO}
import cats.syntax.all.*
import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.catsutils.UnsafeMemoizable.memoize
import js7.base.io.https.Https.loadSSLContext
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.service.Service
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.typeclasses.IsEmpty.syntax.*
import js7.common.http.JsonStreamingSupport
import js7.common.internet.IP.inetSocketAddressShow
import js7.common.pekkohttp.web.PekkoWebServer.{BoundRoute, RouteBinding}
import js7.common.pekkohttp.web.SinglePortPekkoWebServer.*
import js7.common.pekkohttp.web.data.{WebServerBinding, WebServerPort}
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.server.Directives.extractRequest
import org.apache.pekko.http.scaladsl.server.Route
import org.apache.pekko.http.scaladsl.settings.{ParserSettings, ServerSettings}
import org.apache.pekko.http.scaladsl.{ConnectionContext, Http, HttpsConnectionContext}
import org.apache.pekko.stream.TLSClientAuth
import scala.concurrent.Future
import scala.concurrent.duration.{Deadline, FiniteDuration}
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

  override def toString = s"SinglePortPekkoWebServer($binding)"


private object SinglePortPekkoWebServer:
  private val logger = Logger[this.type]

  def resource(
    webServerBinding: WebServerBinding,
    toBoundRoute: RouteBinding => BoundRoute,
    shutdownTimeout: FiniteDuration,
    shutdownDelay: FiniteDuration,
    httpsClientAuthRequired: Boolean)
    (implicit actorSystem: ActorSystem)
  : ResourceIO[SinglePortPekkoWebServer] =
    Resource.suspend:
      memoize: /*saves the `revision` counter for multiple allocations*/
        IO:
          // The revision counter is saved in this memoized IO, see end of this io.
          val revision = Atomic(1)

          def makeBoundRoute(): (BoundRoute, Deferred[IO, Deadline]) =
            val rev = revision.getAndIncrement()
            val terminatingPromise = Deferred.unsafe[IO, Deadline]
            toBoundRoute(RouteBinding(webServerBinding, rev, terminatingPromise)) ->
              terminatingPromise

          resource2(webServerBinding, makeBoundRoute, shutdownTimeout, shutdownDelay,
            httpsClientAuthRequired)

  private def resource2(
    webServerBinding: WebServerBinding,
    makeBoundRoute: () => (BoundRoute, Deferred[IO, Deadline]),
    shutdownTimeout: FiniteDuration,
    shutdownDelay: FiniteDuration,
    httpsClientAuthRequired: Boolean)
    (implicit actorSystem: ActorSystem)
  : ResourceIO[SinglePortPekkoWebServer] =
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
            pekkoBinding <-
              IO
                .fromFuture(IO:
                  // fromFuture is uncancelable!
                  val whenBound = serverBuilder.bind(routeDelegator.webServerRoute)
                  whenBound)
                .flatTap: binding =>
                  IO
                    .fromFuture(IO.pure(binding.whenTerminationSignalIssued))
                    .flatMap(terminatingPromise.complete)
                    .startAndForget
          yield
            // An info line will be logged by DelayedRouteDelegator
            val securityHint = boundRoute.startupSecurityHint(binding.scheme)
            logger.debug(s"$bindingString is bound to $boundRoute$securityHint")
            Binding(binding, pekkoBinding, shutdownTimeout, shutdownDelay, terminatingPromise)
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
            val serviceName = boundRoute.serviceName.ifNonEmpty.fold("")(_ + " ")
            val securityHint = boundRoute.startupSecurityHint(binding.scheme)
            logger.info(s"$name ${serviceName}web services are available$securityHint"))
        .startAndForget

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
    shutdownTimeout: FiniteDuration,
    shutdownDelay: FiniteDuration,
    whenTerminating: Deferred[IO, Deadline]):

    val webServerPort: WebServerPort =
      webServerBinding.toWebServerPort

    def stop: IO[Unit] =
      unbind *>
      logger
        .debugIO(s"$webServerBinding terminate(${shutdownTimeout.pretty})"):
          IO.fromFutureDummyCancelable(IO:
            pekkoBinding.terminate(hardDeadline = shutdownTimeout))
        .void

    // TODO Workaround because terminate(hardDeadline > 0) does not seem to work
    private def unbind: IO[Unit] =
      IO
        .both(
          IO.fromFuture:
            logger.debugIO(s"$webServerBinding unbind"):
              IO(pekkoBinding.unbind()),
          IO(Deadline.now).flatMap: now =>
            whenTerminating.complete(now + shutdownTimeout) *>
              IO.whenA(shutdownDelay.isPositive):
                logger.debugIO(s"$webServerBinding Delay web server termination for ${shutdownDelay.pretty}"):
                  IO.sleep(shutdownDelay))
        .void

    override def toString = webServerPort.toString
