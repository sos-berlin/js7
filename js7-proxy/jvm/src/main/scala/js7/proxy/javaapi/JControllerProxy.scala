package js7.proxy.javaapi

import cats.effect.unsafe.IORuntime
import cats.effect.{IO, Resource, ResourceIO}
import io.vavr.control.Either as VEither
import java.time.Instant
import java.util.Objects.requireNonNull
import java.util.concurrent.CompletableFuture
import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.base.auth.Admission
import js7.base.catsutils.Environment.environment
import js7.base.io.https.HttpsConfig
import js7.base.log.Logger.syntax.*
import js7.base.log.reader.{KeyedLogLine, LogLineKey}
import js7.base.log.{LogLevel, Logger}
import js7.base.problem.Problem
import js7.base.utils.Allocated
import js7.base.utils.CatsUtils.Nel
import js7.data.cluster.ClusterState
import js7.data.controller.ControllerCommand.AddOrdersResponse
import js7.data.event.{Event, EventId}
import js7.data.node.EngineServerId
import js7.data_for_java.common.JavaUtils.Void
import js7.data_for_java.controller.JControllerState
import js7.data_for_java.order.JFreshOrder
import js7.data_for_java.reactor.ReactorConverters.*
import js7.data_for_java.vavr.VavrConverters.*
import js7.proxy.ControllerProxy
import js7.proxy.javaapi.JControllerProxy.*
import js7.proxy.javaapi.data.controller.JEventAndControllerState
import js7.proxy.javaapi.eventbus.JControllerEventBus
import reactor.core.publisher.Flux

/** Observes the Controller's event stream and provides the current JControllerState.
  * After use, stop with `stop()`.
  *
  * Java adapter for `JournaledProxy[JControllerState]`. */
@javaApi
final class JControllerProxy private[proxy](
  allocatedControllerProxy: Allocated[IO, ControllerProxy],
  val api: JControllerApi,
  val controllerEventBus: JControllerEventBus)
  (using ioRuntime: IORuntime):

  private def asScala: ControllerProxy =
    allocatedControllerProxy.allocatedThing

  private val prefetch = api.config.getInt("js7.web.server.prefetch")

  /** Listen to the already running event stream. */
  @Nonnull
  def flux(): Flux[JEventAndControllerState[Event]] =
    asScala.stream()
      .map(JEventAndControllerState.apply)
      .asFlux

  @Nonnull
  def stop(): CompletableFuture[Void] =
    runIO:
      release.as(Void)

  def release: IO[Unit] =
    allocatedControllerProxy.release

  @Nonnull
  def currentState: JControllerState =
    JControllerState(asScala.currentState)

  @Nonnull
  def clusterState: ClusterState =
    asScala.currentState.clusterState

  /** @see [[keyedLogLineFlux]] for a simplified call. */
  def engineLog(serverId: EngineServerId): JResource[JEngineLog] =
    JResource(JEngineLog.resource(this, serverId))

  /** Read log lines from `begin`. */
  @Nonnull
  def keyedLogLineFlux(serverId: EngineServerId, logLevel: LogLevel, begin: Instant, lines: Long)
  : Flux[java.util.List[KeyedLogLine]] =
    keyedLogLineFlux_(serverId, logLevel, begin, lines)

  /** Read log lines beginning after the line denoted by `key`. */
  @Nonnull
  def keyedLogLineFlux(serverId: EngineServerId, logLevel: LogLevel, key: LogLineKey, lines: Long)
  : Flux[java.util.List[KeyedLogLine]] =
    keyedLogLineFlux_(serverId, logLevel, key, lines)

  private def keyedLogLineFlux_(
    serverId: EngineServerId,
    logLevel: LogLevel,
    begin: Instant | LogLineKey,
    lines: Long = Long.MaxValue)
  : Flux[java.util.List[KeyedLogLine]] =
    fs2.Stream.resource:
      JEngineLog.resource(this, serverId)
    .flatMap: jEngineLog =>
      jEngineLog.keyedLogLineStream(logLevel, begin, lines)
    .chunks
    .map(_.asJava)
    .asFlux

  /** Read log lines from `begin`. */
  @Nonnull
  def rawLogLineFlux(serverId: EngineServerId, logLevel: LogLevel, begin: Instant, lines: Long)
  : Flux[java.util.List[Array[Byte]]] =
    rawLogLineFlux_(serverId, logLevel, begin, lines)

  /** Read log lines beginning after the line denoted by `key`. */
  @Nonnull
  def rawLogLineFlux(serverId: EngineServerId, logLevel: LogLevel, key: LogLineKey, lines: Long)
  : Flux[java.util.List[Array[Byte]]] =
    rawLogLineFlux_(serverId, logLevel, key, lines)

  private def rawLogLineFlux_(
    serverId: EngineServerId,
    logLevel: LogLevel,
    begin: Instant | LogLineKey,
    lines: Long = Long.MaxValue)
  : Flux[java.util.List[Array[Byte]]] =
    fs2.Stream.resource:
      JEngineLog.resource(this, serverId)
    .flatMap: jEngineLog =>
      jEngineLog.logSection_(logLevel, begin, lines)
    .map(_.toArray) // Copy, or has Java an immutable array?
    .chunks
    .map(_.asJava)
    .asFlux

  /** Like JControllerApi addOrders, but waits until the Proxy mirrors the added orders. */
  @Nonnull
  def addOrders(@Nonnull orders: Flux[JFreshOrder])
  : CompletableFuture[VEither[Problem, AddOrdersResponse]] =
    runIO:
      asScala.addOrders:
        orders.asFs2Stream(bufferSize = prefetch).map(_.asScala)
      .map(_.toVavr)

  /**
    * Synchronize this mirror with an EventId.
    * The Future completes when this mirror has reached the requested EventId.
    */
  @Nonnull
  def sync(eventId: EventId): CompletableFuture[Void] =
    runIO:
      asScala.sync(eventId)
        .map(_ => Void)

  @Nonnull
  def when(@Nonnull predicate: JEventAndControllerState[Event] => Boolean)
  : CompletableFuture[JEventAndControllerState[Event]] =
    runIO:
      requireNonNull(predicate)
      asScala.when(es => predicate(JEventAndControllerState(es)))
        .map(JEventAndControllerState.apply)

  private def runIO[A](io: IO[A])(using name: sourcecode.Name): CompletableFuture[A] =
    logger.traceIO(name.value):
      io
    .unsafeToCompletableFuture()


object JControllerProxy:
  private val logger = Logger[this.type]

  /** For Scala usage. */
  def resource(
    allocatedControllerProxy: Allocated[IO, ControllerProxy],
    api: JControllerApi,
    controllerEventBus: JControllerEventBus = new JControllerEventBus)
  : ResourceIO[JControllerProxy] =
    Resource.make(
      acquire =
        environment[IORuntime].map: ioRuntime =>
          new JControllerProxy(allocatedControllerProxy, api, controllerEventBus)(using ioRuntime))(
      release = _.release)

  /** Complete Resource with [[JProxyContext]] and [[JControllerApi]] — for Scala usage. */
  def completeResource(
    admissions: Nel[Admission],
    httpsConfig: HttpsConfig = HttpsConfig.empty)
  : ResourceIO[JControllerProxy] =
    for
      jContext <- JProxyContext.resource()
      jControllerApi <- jContext.controllerApiResource(admissions, httpsConfig)
      jProxy <- jControllerApi.proxyResource()
    yield
      jProxy
