package js7.common.akkahttp.web

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.Route
import cats.data.NonEmptySeq
import cats.effect.Resource
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
import js7.base.monixutils.AsyncVariable
import js7.base.problem.Problems.WebServiceStillNotAvailableProblem
import js7.base.service.Service
import js7.base.thread.IOExecutor
import js7.base.time.ScalaTime.{DurationRichInt, RichFiniteDuration}
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Allocated, DelayConf, Delayer}
import js7.base.web.Uri
import js7.common.akkahttp.StandardMarshallers.*
import js7.common.akkahttp.web.AkkaWebServer.{BindingAndResource, *}
import js7.common.akkahttp.web.data.WebServerBinding
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import monix.eval.Task
import org.jetbrains.annotations.TestOnly
import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.duration.{Deadline, FiniteDuration}
import scala.util.Try

/**
 * @author Joacim Zschimmer
 */
final class AkkaWebServer private(bindingAndResources: Vector[BindingAndResource], config: Config)
  (implicit testEventBus: StandardEventBus[Any])
extends WebServerBinding.HasLocalUris
with Service.StoppableByRequest
{
  private val _addrToHttpsFileToTime = mutable.Map.empty[InetSocketAddress, Map[Path, Try[FileTime]]]

  private[web] val webServerBindings =
    bindingAndResources.map(_.webServerBinding)

  protected val webServerPorts =
    bindingAndResources.map(_.webServerBinding.toWebServerPort)

  private val portWebServerAllocated =
    AsyncVariable[Vector[Option[Allocated[Task, SinglePortAkkaWebServer]]]](Vector.empty)

  protected def start =
    bindingAndResources
      .traverse(_.resource.toAllocated.map(Some(_)))
      .flatMap(portWebServerAllocated.set)
      .*>(startService(
        untilStopRequested))

  def restartWhenHttpsChanges(implicit iox: IOExecutor): Resource[Task,Unit] =
    HttpsDirectoryWatch
      .resource(
        DirectoryWatchSettings.fromConfig(config).orThrow,
        webServerBindings.flatMap(_.requiredFiles),
        onHttpsKeyOrCertChanged)
      .void

  private[web] def onHttpsKeyOrCertChanged(implicit iox: IOExecutor): Task[Unit] =
    logger.debugTask(Task.defer {
      testEventBus.publish(BeforeRestartEvent)

      portWebServerAllocated
        .update(sequence =>
          sequence
            .zip(bindingAndResources)
            .traverse((checkFilesThenRestart _).tupled))
        .void
    })

  private def checkFilesThenRestart(
    previouslyAllocated: Option[Allocated[Task, SinglePortAkkaWebServer]],
    bindingAndResource: BindingAndResource)
    (implicit iox: IOExecutor)
  : Task[Option[Allocated[Task, SinglePortAkkaWebServer]]] =
    Task.defer {
      val binding = bindingAndResource.webServerBinding
      readFileTimes(binding).flatMap(fileToTime =>
        if (fileToTime == _addrToHttpsFileToTime.getOrElse(binding.address, Map.empty)) {
          if (fileToTime.nonEmpty) {
            logger.debug(s"onHttpsKeyOrCertChanged but no change detected: $binding ${
              fileToTime.mkString(", ")}")
          }
          Task.pure(previouslyAllocated)
        } else {
          logger.info(
            s"Restart HTTPS web server due to changed HTTPs keys or certificates: ${
              fileToTime.view.mapValues(_.fold(_.toString, _.toString)).mkString(", ")
            }")
          previouslyAllocated.fold(Task.unit)(_.release) *>
            startPortWebServer(bindingAndResource) <*
            Task(testEventBus.publish(RestartedEvent))
        })
    }

  private def startPortWebServer(bindingAndResource: BindingAndResource)(implicit iox: IOExecutor)
  : Task[Option[Allocated[Task, SinglePortAkkaWebServer]]] = {
    import bindingAndResource.{resource, webServerBinding as binding}

    Delayer.start[Task](delayConf)
      .flatMap(delayer =>
        readFileTimes(binding)
          .flatMap(fileTimes => resource.toAllocated.map(allo => (fileTimes -> allo).some))
          .onErrorRestartLoop(()) {
            case ((throwable, _, retry)) =>
              logger.error(s"$bindingAndResource => ${throwable.toStringWithCauses}")
              for (t <- throwable.ifStackTrace) logger.debug(s"💥 ${t.toStringWithCauses}", t)
              Task
                .race(
                  untilStopRequested,
                  delayer.sleep(logDelay(_, bindingAndResource.toString)))
                .flatMap(_.fold(_ => Task.none/*stop requested*/, _ => retry(())))
          }
          .map(_.map { case (fileTimes, allocated) =>
            _addrToHttpsFileToTime(binding.address) = fileTimes
            allocated
          }))
  }

  private def readFileTimes(binding: WebServerBinding)(implicit iox: IOExecutor)
  : Task[Map[Path, Try[FileTime]]] =
    Task(
      binding.requiredFiles
        .map(file => file -> Try(Files.getLastModifiedTime(file)))
        .toMap
    ).executeOn(iox.scheduler)

  private def logDelay(duration: FiniteDuration, name: String) =
    Task(logger.info(
      s"Due to failure, $name restarts ${
        if (duration.isZero) "now" else "in " + duration.pretty}"))

  override def toString =
    s"AkkaWebServer(${webServerPorts mkString " "})"
}

object AkkaWebServer
{
  private val logger = Logger[this.type]
  private val delayConf = DelayConf(NonEmptySeq.of(1.s, 3.s, 6.s, 10.s))

  private[web] val testConfig = config"""
    js7.web.server.auth.https-client-authentication = off
    js7.web.server.shutdown-timeout = 10s
    """

  @TestOnly
  def testResource()(route: Route)(implicit as: ActorSystem): Resource[Task, AkkaWebServer] =
    testUriAndResource()(route)._2

  @TestOnly
  def testUriAndResource()(route: Route)(implicit as: ActorSystem)
  : (Uri, Resource[Task, AkkaWebServer]) =
    testResource(ConfigFactory.empty)(route)

  @TestOnly
  def testResource(config: Config)(route: Route)(implicit as: ActorSystem)
  : (Uri, Resource[Task, AkkaWebServer]) =
    testResource(findFreeTcpPort(), config, route = route)

  @TestOnly
  def testResource(port: Int = findFreeTcpPort(), config: Config = ConfigFactory.empty, route: Route)(implicit as: ActorSystem)
  : (Uri, Resource[Task, AkkaWebServer]) =
    Uri(s"http://127.0.0.1:$port") -> httpResource(port = port, config.withFallback(testConfig), route)

  def httpResource(port: Int, config: Config, route: Route)
    (implicit actorSystem: ActorSystem)
  : Resource[Task, AkkaWebServer] =
    resource(
      Seq(WebServerBinding.http(port)),
      config,
      (_, _) => BoundRoute.simple(route))

  def resource(
    webServerBindings: Seq[WebServerBinding],
    config: Config,
    toBoundRoute: (WebServerBinding, Future[Deadline]) => BoundRoute)
    (implicit actorSystem: ActorSystem,
      testEventBus: StandardEventBus[Any] = new StandardEventBus)
  : Resource[Task, AkkaWebServer] =
    Resource.suspend(Task {
      val shutdownTimeout = config.finiteDuration("js7.web.server.shutdown-timeout").orThrow
      val httpsClientAuthRequired = config.getBoolean(
        "js7.web.server.auth.https-client-authentication")

      Service
        .resource(Task(
          new AkkaWebServer(
            for (webServerBinding <- webServerBindings.toVector) yield
              BindingAndResource(
                webServerBinding,
                SinglePortAkkaWebServer
                  .resource(
                    webServerBinding,
                    toBoundRoute(webServerBinding, _),
                    shutdownTimeout = shutdownTimeout,
                    httpsClientAuthRequired = httpsClientAuthRequired)),
            config)))
    })

  private lazy val stillNotAvailableRoute: Route =
    complete(WebServiceStillNotAvailableProblem)

  private final case class BindingAndResource(
    webServerBinding: WebServerBinding,
    resource: Resource[Task, SinglePortAkkaWebServer])
  {
    override def toString = webServerBinding.toString
  }

  trait BoundRoute {
    def serviceName: String

    def stillNotAvailableRoute: Route =
      AkkaWebServer.stillNotAvailableRoute

    def webServerRoute: Task[Route]

    def startupSecurityHint(scheme: WebServerBinding.Scheme): String
  }
  object BoundRoute {
    def simple(route: Route): BoundRoute =
      new Simple(route)

    final class Simple(route: Route) extends BoundRoute {
      def serviceName = ""

      def webServerRoute = Task.pure(route)

      def startupSecurityHint(scheme: WebServerBinding.Scheme) = ""
    }
  }

  /** Event only for testing with EventBus. */
  object BeforeRestartEvent

  /** Event only for testing with EventBus. */
  object RestartedEvent
}
