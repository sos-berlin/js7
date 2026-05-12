package js7.proxy.javaapi

import cats.effect.unsafe.IORuntime
import cats.effect.{IO, Resource, ResourceIO, SyncIO}
import cats.syntax.option.*
import com.typesafe.config.{Config, ConfigFactory}
import java.lang.Thread.currentThread
import java.nio.file.Path
import java.util.Optional
import java.util.concurrent.{CompletableFuture, Executor, ForkJoinPool}
import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.base.auth.Admission
import js7.base.catsutils.CatsEffectExtensions.{run, unsafeRuntime}
import js7.base.catsutils.{OurIORuntime, OurIORuntimeRegister}
import js7.base.config.Js7Conf
import js7.base.io.https.HttpsConfig
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.log.reader.LogDirectoryMXBean
import js7.base.system.startup.StartUp
import js7.base.thread.CatsBlocking.syntax.awaitInfinite
import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.:=
import js7.base.utils.CatsUtils.*
import js7.base.utils.Tests.isTest
import js7.common.message.ProblemCodeMessages
import js7.common.pekkoutils.Pekkos
import js7.common.system.startup.MainSupportService
import js7.controller.client.PekkoHttpControllerApi.admissionsToApiResource
import js7.data_for_java.auth.{JAdmission, JHttpsConfig}
import js7.data_for_java.common.JavaUtils.{-->, Void}
import js7.proxy.configuration.ProxyConfs
import js7.proxy.data.GroupAndProxyId
import js7.proxy.javaapi.JProxyContext.*
import js7.proxy.{ControllerApi, ControllerApiRegister, MetricsForServlet}
import org.apache.pekko.actor.ActorSystem
import org.jetbrains.annotations.TestOnly
import scala.annotation.static
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

/** Essentially provides a Cats Effect IORuntime for the Proxy API.
  *
  * @param groupAndProxyId Required for Prometheus metrics. Call makeSingleton, too!
  */
final class JProxyContext private(
  groupAndProxyId: Option[GroupAndProxyId],
  val config: Config,
  actorSystem: ActorSystem)
  (using val ioRuntime: IORuntime)
extends AutoCloseable:

  private val proxyConf = ProxyConfs.fromConfig(config)
  private val _controllerApiRegister = new ControllerApiRegister(groupAndProxyId)
  private var releaseOuter: IO[Unit] = IO.unit
  private var releaseIORuntime: SyncIO[Unit] = SyncIO.unit
  private val releaseLogDirectoryMBean = Atomic(SyncIO.unit)

  private def setReleaser(release: IO[Unit], releaseIORuntime: SyncIO[Unit]): Unit =
    this.releaseOuter = release
    this.releaseIORuntime = releaseIORuntime

  /** Blockingly release this `JProxyContext`.*/
  def close(): Unit =
    release().get()

  /** Asynchronously release this `JProxyContext`.*/
  def release(): CompletableFuture[Void] =
    releaseOuterResources
      .guarantee:
        releaseMe
      .unsafeToCompletableFuture()
      .thenApplyAsync(
        _ =>
          releaseIORuntime.run()
          Void,
        ForkJoinPool.commonPool)

  private def releaseOuterResources: IO[Unit] =
    logger.debugIO("releaseOuterResources"):
      IO.defer:
        _singleton := None
        releaseOuter

  private def releaseMe =
    // Don't release outer resources here
    IO:
      _singleton := None
    .guarantee:
      releaseLogDirectoryMBean.getAndSet(SyncIO.unit).to[IO]

  /** Make this one and only `JProxyContext` statically known.
    *
    * Then, ProxyMetricsServlet can read the registered [[ControllerApi]]s.
    *
    * Usefull only, if groupAndProxyId has been set.
    */
  def makeSingleton(): Unit =
    _singleton.getAndSet(Some(this)).foreach: former =>
      logger.error(s"Second call to makeSingleton(): $this, former was: $former")
      if isTest then throw new IllegalStateException("Second call to makeSingleton")

  def registerLogDirectoryMBean(logDirectory: Path): Unit =
    val release = LogDirectoryMXBean.register[SyncIO](logDirectory).allocated.run()._2
    val former = releaseLogDirectoryMBean.getAndSet(release)
    former.run()

  def metricsForServlet: MetricsForServlet =
    _controllerApiRegister

  /** Runs `body` with an own [[JControllerApi]].
    */
  @javaApi
  def runControllerApi[A](
    admissions: java.lang.Iterable[JAdmission],
    httpsConfig: JHttpsConfig,
    body: JControllerApi --> CompletableFuture[A])
  : CompletableFuture[A] =
    CompletableFuture.supplyAsync: () =>
      newControllerApi(admissions, httpsConfig)
    .thenCompose: jControllerApi =>
      body(jControllerApi)
        .thenCompose: result =>
          jControllerApi.stop().thenApply(_ => result)
        .exceptionallyCompose: throwable =>
          jControllerApi.stop().thenCompose: _ =>
            CompletableFuture.failedFuture(throwable)

  /** For Scala usage. */
  def controllerApiResource(
    admissions: Nel[Admission],
    httpsConfig: HttpsConfig = HttpsConfig.empty)
  : ResourceIO[JControllerApi] =
    Resource.make(
      acquire = IO:
        newControllerApi(
          admissions.toList.map(JAdmission(_)).asJava,
          JHttpsConfig(httpsConfig)))(
      release = api =>
        IO.fromCompletableFuture:
          IO:
            api.stop()
        .void)

  /**
    * @param proxyId Set only if this is the only JControllerApi in the JVM.
    *                Don't use in tests (except one)!
    */
  @javaApi @Nonnull
  def newControllerApi(
    @Nonnull admissions: java.lang.Iterable[JAdmission],
    @Nonnull httpsConfig: JHttpsConfig = JHttpsConfig.empty)
  : JControllerApi =
    val apiResource = admissionsToApiResource(
      admissions = Nel.unsafe(admissions.asScala.map(_.asScala).toList),
      httpsConfig.asScala
    )(using actorSystem)
    val api = new ControllerApi(apiResource, proxyConf, Some(_controllerApiRegister))
    new JControllerApi(api, config)

  override def toString = s"JProxyContext($groupAndProxyId)"


object JProxyContext:

  Logger.dontInitialize()
  @static private val ThreadPoolName = "JS7-Proxy"
  @static val ThreadNamePrefix: String = s"$ThreadPoolName-"
  private lazy val logger = Logger[this.type]
  private val _singleton = Atomic(none[JProxyContext])

  def metricsForServlet: Option[MetricsForServlet] =
    _singleton.get().map(_.metricsForServlet)

  /** Runs `body` with an own [[JProxyContext]]. */
  @javaApi
  def run[A](
    groupAndProxyId: Optional[GroupAndProxyId],
    config: Config,
    body: JProxyContext --> CompletableFuture[A])
  : CompletableFuture[A] =
    start(groupAndProxyId, config)
      .thenCompose: jProxyContext =>
        body(jProxyContext)
          .thenCompose: result =>
            jProxyContext.release().thenApply(_ => result)
          .exceptionallyCompose: throwable =>
            jProxyContext.release().thenCompose: _ =>
              CompletableFuture.failedFuture(throwable)

  @TestOnly
  def apply(groupAndProxyId: Option[GroupAndProxyId] = None, config: Config = ConfigFactory.empty)(using IORuntime)
  : JProxyContext =
    resource(groupAndProxyId, config = config)
      .allocated.map: (jCtx, release) =>
        jCtx.setReleaser(release, SyncIO.unit)
        jCtx
      .awaitInfinite

  @javaApi
  def start(): CompletableFuture[JProxyContext] =
    start(Optional.empty(), ConfigFactory.empty())

  @javaApi
  def start(config: Config)
  : CompletableFuture[JProxyContext] =
    start(Optional.empty(), config)

  @javaApi
  def start(groupAndProxyId: Optional[GroupAndProxyId], config: Config)
  : CompletableFuture[JProxyContext] =
    val ((ioRuntime, resource), releaseRuntime) =
      resourceWithIORuntime(groupAndProxyId.toScala, config).allocated.run()
    given IORuntime = ioRuntime
    resource.allocated.map: (jCtx, release) =>
      jCtx.setReleaser(release, releaseRuntime)
      jCtx
    .unsafeToCompletableFuture()

  private def resourceWithIORuntime(
    groupAndProxyId: Option[GroupAndProxyId] = None,
    config: Config = ConfigFactory.empty,
    computeExecutor: Option[Executor] = None)
  : Resource[SyncIO, (IORuntime, ResourceIO[JProxyContext])] =
    val config_ = configWithDefaults(config)
    for
      _ <- Resource.eval:
        SyncIO:
          Logger.dontInitialize()
          logger.info(StartUp.startUpLine("JS7 Proxy"))
          ProblemCodeMessages.initialize()
      ioRuntime <-
        OurIORuntime.resource[SyncIO](ThreadPoolName, config_, computeExecutor = computeExecutor)
    yield
      ioRuntime -> resource_(groupAndProxyId, config_)

  def resource(
    groupAndProxyId: Option[GroupAndProxyId] = None,
    config: Config = ConfigFactory.empty)
  : ResourceIO[JProxyContext] =
    resource_(groupAndProxyId, configWithDefaults(config))

  private def resource_(
    groupAndProxyId: Option[GroupAndProxyId] = None,
    config: Config = ConfigFactory.empty)
  : ResourceIO[JProxyContext] =
    Resource.suspend:
      IO.unsafeRuntime.map: ioRuntime =>
        given IORuntime = ioRuntime
        for
          env = OurIORuntimeRegister.toEnvironment(ioRuntime)
          _ <- Js7Conf.registerInEnvironment[IO](env, config)
          actorSystem <- Pekkos.actorSystemResource("JS7-Proxy", config)
          _ <- MainSupportService.service(config, useOwnResponsivenessMeter = true)
          result <- Resource.make(
            acquire = IO(new JProxyContext(groupAndProxyId, config, actorSystem)))(
            release = _.releaseMe)
        yield
          result

  private def configWithDefaults(config: Config): Config =
    config
      .withFallback(ConfigFactory.systemProperties)
      .withFallback(ProxyConfs.defaultConfig)

  def assertIsNotProxyThread(): Unit =
    assert(!currentThread.getName.startsWith(ThreadNamePrefix), "Running in a Proxy thread")

  def assertIsProxyThread(): Unit =
    val threadName = currentThread.getName
    assert(threadName.startsWith(ThreadNamePrefix), s"Not running in a Proxy thread: $threadName")
