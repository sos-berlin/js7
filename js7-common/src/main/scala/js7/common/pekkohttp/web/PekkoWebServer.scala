package js7.common.pekkohttp.web

import cats.effect.Deferred
import cats.effect.{IO, Resource, ResourceIO}
import cats.instances.vector.*
import cats.syntax.all.*
import com.typesafe.config.{Config, ConfigFactory}
import java.net.InetSocketAddress
import java.nio.file.attribute.FileTime
import java.nio.file.{Files, Path}
import js7.base.configutils.Configs.*
import js7.base.eventbus.StandardEventBus
import js7.base.io.file.watch.DirectoryWatchSettings
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixlike.MonixLikeExtensions.onErrorRestartLoop
import js7.base.monixutils.AsyncVariable
import js7.base.problem.Problems.WebServiceStillNotAvailableProblem
import js7.base.service.Service
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Allocated, DelayConf, Delayer}
import js7.base.web.Uri
import js7.common.pekkohttp.StandardMarshallers.*
import js7.common.pekkohttp.web.PekkoWebServer.*
import js7.common.pekkohttp.web.data.WebServerBinding
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.server.Directives.complete
import org.apache.pekko.http.scaladsl.server.Route
import org.jetbrains.annotations.TestOnly
import scala.collection.mutable
import scala.concurrent.duration.{Deadline, FiniteDuration}
import scala.util.{Failure, Success, Try}

/**
 * @author Joacim Zschimmer
 */
final class PekkoWebServer private(bindingAndResources: Vector[BindingAndResource], config: Config)
  (implicit testEventBus: StandardEventBus[Any])
extends WebServerBinding.HasLocalUris, Service.StoppableByRequest:

  private val _addrToHttpsFileToTime = mutable.Map.empty[InetSocketAddress, Map[Path, Try[FileTime]]]

  private[web] val webServerBindings =
    bindingAndResources.map(_.webServerBinding)

  protected val webServerPorts =
    bindingAndResources.map(_.webServerBinding.toWebServerPort)

  private val portWebServersAllocated =
    AsyncVariable[Vector[Option[Allocated[IO, SinglePortPekkoWebServer]]]](Vector.empty)

  protected def start =
    bindingAndResources
      .traverse(_.resource.toAllocated.map(Some(_)))
      .flatMap(portWebServersAllocated.set)
      .*>(startService:
        untilStopRequested
          .guarantee(stopPortWebServers))


  def stopPortWebServers: IO[Unit] =
    logger.traceIO:
      portWebServersAllocated.value
        .map(_.flatMap(_.map(_.release)))
        .flatMap(_.parSequence)
        .map(_.combineAll)

  def restartWhenHttpsChanges: ResourceIO[Unit] =
    HttpsDirectoryWatch
      .resource(
        DirectoryWatchSettings.fromConfig(config).orThrow,
        webServerBindings.flatMap(_.requiredFiles),
        onHttpsKeyOrCertChanged)
      .void

  private[web] def onHttpsKeyOrCertChanged: IO[Unit] =
    logger.debugIO(IO.defer:
      testEventBus.publish(BeforeRestartEvent)

      portWebServersAllocated
        .update: sequence =>
          sequence
            .zip(bindingAndResources)
            .parTraverse((checkFilesThenRestart _).tupled)
        .void)

  private def checkFilesThenRestart(
    previouslyAllocated: Option[Allocated[IO, SinglePortPekkoWebServer]],
    bindingAndResource: BindingAndResource)
  : IO[Option[Allocated[IO, SinglePortPekkoWebServer]]] =
    IO.defer:
      val binding = bindingAndResource.webServerBinding
      readFileTimes(binding).flatMap: fileToTime =>
        val prevFileToTime = _addrToHttpsFileToTime.getOrElse(binding.address, Map.empty)
        val diff = fileToTime.filterNot:
          case (file, Failure(_)) => prevFileToTime.get(file).exists(_.isFailure)
          case (file, t @ Success(_)) => prevFileToTime.get(file).contains(t)
        (if diff.isEmpty then
          if fileToTime.nonEmpty then logger.debug(s"onHttpsKeyOrCertChanged but no change detected")
          IO.pure(previouslyAllocated)
        else
          logger.info(s"Restart HTTPS web server due to changed keys or certificates: ${
            diff.view.mapValues(_.fold(_.toString, _.toString)).mkString(", ")}")
          previouslyAllocated.fold(IO.unit)(_.release) *>
            startPortWebServer(bindingAndResource) <*
            IO(testEventBus.publish(RestartedEvent))
        ).<*(IO:
          _addrToHttpsFileToTime(binding.address) = prevFileToTime)

  private def startPortWebServer(bindingAndResource: BindingAndResource)
  : IO[Option[Allocated[IO, SinglePortPekkoWebServer]]] =
    import bindingAndResource.{resource, webServerBinding as binding}

    Delayer.start[IO](DelayConf.default)
      .flatMap: delayer =>
        var errorLogged = false
        readFileTimes(binding)
          .flatMap: fileTimes =>
            resource.toAllocated
              .map(allo => (fileTimes -> allo).some)
          .onErrorRestartLoop(()):
            case (throwable, _, retry) =>
              errorLogged = true
              logger.error:
                s"🔴 Web server for $bindingAndResource: ${throwable.toStringWithCauses}"
              for t <- throwable.ifStackTrace do logger.debug(s"💥 ${t.toStringWithCauses}", t)
              IO
                .race(
                  untilStopRequested,
                  delayer.sleep(logDelay(_, bindingAndResource.toString)))
                .flatMap(_.fold(_ => IO.none/*stop requested*/, _ => retry(())))
          .map(_.map: (fileTimes, allocated) =>
            _addrToHttpsFileToTime(binding.address) = fileTimes
            if errorLogged then logger.info(s"🟢 Web server for $bindingAndResource restarted")
            allocated)

  private def readFileTimes(binding: WebServerBinding)
  : IO[Map[Path, Try[FileTime]]] =
    IO.interruptible:
      binding.requiredFiles
        .map(file => file -> Try(Files.getLastModifiedTime(file)))
        .toMap

  private def logDelay(duration: FiniteDuration, name: String) =
    IO(logger.debug(
      s"Restart $name ${if duration.isZero then "now" else "in " + duration.pretty} due to failure"))

  override def toString =
    s"PekkoWebServer(${webServerPorts mkString " "})"


object PekkoWebServer:
  private val logger = Logger[this.type]

  @TestOnly
  private val testConfig = config"""
    js7.web.server.auth.https-client-authentication = off
    js7.web.server.shutdown-timeout = 10s
    js7.web.server.shutdown-delay = 500ms
    """

  @TestOnly
  def testResource()(route: Route)(using ActorSystem): ResourceIO[PekkoWebServer] =
    testUriAndResource()(route)._2

  @TestOnly
  def testUriAndResource()(route: Route)(using ActorSystem)
  : (Uri, ResourceIO[PekkoWebServer]) =
    testResource(ConfigFactory.empty)(route)

  @TestOnly
  private def testResource(config: Config)(route: Route)(using ActorSystem)
  : (Uri, ResourceIO[PekkoWebServer]) =
    testResource(findFreeTcpPort(), config, route = route)

  @TestOnly
  private def testResource(
    port: Int = findFreeTcpPort(),
    config: Config = ConfigFactory.empty,
    route: Route)
    (using ActorSystem)
  : (Uri, ResourceIO[PekkoWebServer]) =
    Uri(s"http://127.0.0.1:$port") ->
      httpResource(port = port, config.withFallback(testConfig), route)

  def httpResource(port: Int, config: Config, route: Route)(using ActorSystem)
  : ResourceIO[PekkoWebServer] =
    resource(
      Seq(WebServerBinding.http(port)),
      config,
      _ => BoundRoute.simple(route))

  def resource(
    webServerBindings: Seq[WebServerBinding],
    config: Config,
    toBoundRoute: RouteBinding => BoundRoute)
    (using actorSystem: ActorSystem,
      testEventBus: StandardEventBus[Any] = new StandardEventBus)
  : ResourceIO[PekkoWebServer] =
    Resource.suspend(IO:
      val shutdownTimeout = config.finiteDuration("js7.web.server.shutdown-timeout").orThrow
      val shutdownDelay = config.finiteDuration("js7.web.server.shutdown-delay").orThrow
      val httpsClientAuthRequired = config.getBoolean(
        "js7.web.server.auth.https-client-authentication")

      Service.resource(IO:
        new PekkoWebServer(
          for webServerBinding <- webServerBindings.toVector yield
            BindingAndResource(
              webServerBinding,
              SinglePortPekkoWebServer.resource(
                webServerBinding,
                toBoundRoute(_),
                shutdownTimeout = shutdownTimeout,
                shutdownDelay = shutdownDelay,
                httpsClientAuthRequired = httpsClientAuthRequired)),
          config)))

  private lazy val stillNotAvailableRoute: Route =
    complete(WebServiceStillNotAvailableProblem)

  private final case class BindingAndResource(
    webServerBinding: WebServerBinding,
    resource: ResourceIO[SinglePortPekkoWebServer]):
    override def toString = webServerBinding.toString

  final case class RouteBinding private[web](
    webServerBinding: WebServerBinding,
    /** revision distinguishes a second RouteBinding from the first one, when the Route is
     * established anew due to changed HTTPS key or certificate. */
    revision: Int,
    whenStopRequested: Deferred[IO, Deadline])

  trait BoundRoute:
    def serviceName: String

    def stillNotAvailableRoute: Route =
      PekkoWebServer.stillNotAvailableRoute

    def webServerRoute: IO[Route]

    def startupSecurityHint(scheme: WebServerBinding.Scheme): String
  object BoundRoute:
    def simple(route: Route): BoundRoute =
      new Simple(route)

    final class Simple(route: Route) extends BoundRoute:
      val serviceName = ""

      val webServerRoute: IO[Route] = IO.pure(route)

      def startupSecurityHint(scheme: WebServerBinding.Scheme) = ""

  /** Event only for testing with EventBus. */
  object BeforeRestartEvent

  /** Event only for testing with EventBus. */
  object RestartedEvent
