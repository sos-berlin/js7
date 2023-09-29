package js7.common.akkahttp.web

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives.extractRequest
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.settings.{ParserSettings, ServerSettings}
import akka.http.scaladsl.{ConnectionContext, Http, HttpsConnectionContext}
import akka.stream.TLSClientAuth
import cats.effect.Resource
import cats.syntax.all.*
import js7.base.io.https.Https.loadSSLContext
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.service.Service
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.typeclasses.IsEmpty.syntax.toIsEmptyAllOps
import js7.common.akkahttp.web.AkkaWebServer.{BoundRoute, RouteBinding}
import js7.common.akkahttp.web.SinglePortAkkaWebServer.*
import js7.common.akkahttp.web.data.{WebServerBinding, WebServerPort}
import js7.common.http.JsonStreamingSupport
import js7.common.internet.IP.inetSocketAddressShow
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.atomic.Atomic
import scala.concurrent.duration.{Deadline, FiniteDuration}
import scala.concurrent.{Future, Promise}
import scala.util.chaining.scalaUtilChainingOps
import scala.util.{Failure, Success}

/**
 * @author Joacim Zschimmer
 */
private final class SinglePortAkkaWebServer private(binding: Binding)
  (implicit protected val actorSystem: ActorSystem)
extends Service.StoppableByRequest
{
  protected def start =
    startService(
      untilStopRequested *> onStop)

  private def onStop: Task[Unit] =
    binding.stop
      //.onErrorHandle(t =>
      //  logger.error(s"$toString $binding.terminate => ${t.toStringWithCauses}",
      //    t.nullIfNoStackTrace))

  override def toString = s"SinglePortAkkaWebServer($binding)"
}

private object SinglePortAkkaWebServer
{
  private val logger = Logger[this.type]

  def resource(
    webServerBinding: WebServerBinding,
    toBoundRoute: RouteBinding => BoundRoute,
    shutdownTimeout: FiniteDuration,
    httpsClientAuthRequired: Boolean)
    (implicit actorSystem: ActorSystem)
  : Resource[Task, SinglePortAkkaWebServer] =
    Resource.suspend(Task {
      // The revision counter is saved in this memoized Task, see end of this task.
      val revision = Atomic(1)

      def makeBoundRoute(): (BoundRoute, Promise[Deadline]) = {
        val rev = revision.getAndIncrement()
        val terminatingPromise = Promise[Deadline]()
        toBoundRoute(RouteBinding(webServerBinding, rev, terminatingPromise.future)) ->
          terminatingPromise
      }

      resource2(webServerBinding, makeBoundRoute _, shutdownTimeout, httpsClientAuthRequired)
    }.memoize/*saves the `revision` counter for multiple allocations*/)

  private def resource2(
    webServerBinding: WebServerBinding,
    makeBoundRoute: () => (BoundRoute, Promise[Deadline]),
    shutdownTimeout: FiniteDuration,
    httpsClientAuthRequired: Boolean)
    (implicit actorSystem: ActorSystem)
  : Resource[Task, SinglePortAkkaWebServer] =
    Service.resource(Task.defer {
      val akkaHttp = Http(actorSystem)

      def bindHttps(https: WebServerBinding.Https): Task[Binding] = {
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
      : Task[Binding] =
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

          val (boundRoute, terminatingPromise) = makeBoundRoute()
          val bindingString = s"${binding.scheme}://${binding.address.show}"
          Task
            .deferFutureAction { implicit scheduler =>
              val routeDelegator = new DelayedRouteDelegator(binding, boundRoute, bindingString)
              val whenBound = serverBuilder.bind(routeDelegator.webServerRoute)
              terminatingPromise.completeWith(whenBound.flatMap(_.whenTerminationSignalIssued))
              whenBound
            }
            .<*(Task {
              // An info line will be logged by DelayedRouteDelegator
              val securityHint = boundRoute.startupSecurityHint(binding.scheme)
              logger.debug(s"$bindingString is bound to $boundRoute$securityHint")
            })
            .map(Binding(binding, _, shutdownTimeout))
        }

      webServerBinding
        .match {
          case o: WebServerBinding.Http => bind(o)
          case o: WebServerBinding.Https => bindHttps(o)
        }
        .map(binding =>
          new SinglePortAkkaWebServer(binding))
    })

  /** Returns 503 ServiceUnavailable until the Route is provided. */
  private final class DelayedRouteDelegator(
    binding: WebServerBinding,
    boundRoute: BoundRoute,
    name: String)
    (implicit scheduler: Scheduler)
  {
    private val _realRoute = Atomic(none[Route])

    private val whenRealRoute: Future[Route] =
      boundRoute.webServerRoute
        .tapEval(realRoute => Task {
          if _realRoute.compareAndSet(None, Some(realRoute)) then {
            val serviceName = boundRoute.serviceName.emptyToNone.fold("")(_ + " ")
            val securityHint = boundRoute.startupSecurityHint(binding.scheme)
            logger.info(s"$name ${serviceName}web services are available$securityHint")
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

  private final case class Binding(
    webServerBinding: WebServerBinding,
    akkaBinding: Http.ServerBinding,
    shutdownTimeout: FiniteDuration)
  {
    val webServerPort: WebServerPort =
      webServerBinding.toWebServerPort

    def stop: Task[Unit] =
      logger
        .debugTask(s"Terminate $toString")(
          Task.deferFuture(
            akkaBinding.terminate(hardDeadline = shutdownTimeout)))
        .void

    override def toString = webServerPort.toString
  }
}
