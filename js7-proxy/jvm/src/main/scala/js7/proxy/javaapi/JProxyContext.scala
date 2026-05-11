package js7.proxy.javaapi

import cats.effect.unsafe.IORuntime
import cats.effect.{IO, Resource, ResourceIO, SyncIO}
import cats.syntax.flatMap.*
import cats.syntax.option.*
import com.typesafe.config.{Config, ConfigFactory}
import java.lang.Thread.currentThread
import java.util.Optional
import java.util.concurrent.{CompletableFuture, Executor}
import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.base.auth.Admission
import js7.base.catsutils.CatsEffectExtensions.run
import js7.base.catsutils.{OurIORuntime, OurIORuntimeRegister}
import js7.base.config.Js7Conf
import js7.base.io.https.HttpsConfig
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.system.startup.StartUp
import js7.base.thread.CatsBlocking.syntax.awaitInfinite
import js7.base.utils.Atomic.extensions.:=
import js7.base.utils.CatsUtils.*
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
import js7.base.utils.Nulls.nullToNone
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isTest
import js7.base.utils.{Atomic, Lazy}
import js7.common.message.ProblemCodeMessages
import js7.common.pekkoutils.Pekkos
import js7.common.pekkoutils.Pekkos.newActorSystem
import js7.common.system.startup.MainSupportService
import js7.controller.client.PekkoHttpControllerApi.admissionsToApiResource
import js7.data_for_java.auth.{JAdmission, JHttpsConfig}
import js7.data_for_java.common.JavaUtils.{-->, Void}
import js7.proxy.configuration.ProxyConfs
import js7.proxy.data.GroupAndProxyId
import js7.proxy.javaapi.JProxyContext.*
import js7.proxy.{ControllerApi, ControllerApiRegister, MetricsForServlet}
import scala.annotation.static
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

/** Essentially provides a Cats Effect IORuntime for the Proxy API.
  *
  * @param groupAndProxyId Required for Prometheus metrics. Call makeSingleton, too!
  */
final class JProxyContext private(
  groupAndProxyId: Option[GroupAndProxyId] = None,
  config_ : Config = ConfigFactory.empty(),
  computeExecutor: Option[Executor] = None)
extends AutoCloseable:

  def this() =
    this(None, ConfigFactory.empty, None)

  def this(groupAndProxyId: Optional[GroupAndProxyId]) =
    this(groupAndProxyId.toScala, ConfigFactory.empty, None)

  def this(groupAndProxyId: Optional[GroupAndProxyId], config: Config) =
    this(groupAndProxyId.toScala, config, None)

  def this(
    groupAndProxyId: Optional[GroupAndProxyId],
    config: Config,
    computeExecutor: Executor | Null)
  =
    this(groupAndProxyId.toScala, config, nullToNone(computeExecutor))

  val config: Config =
    config_
      .withFallback(ConfigFactory.systemProperties)
      .withFallback(ProxyConfs.defaultConfig)

  private val proxyConf = ProxyConfs.fromConfig(config)

  private val (ioRuntime, ioRuntimeShutdown) =
    OurIORuntime.resource[SyncIO](ThreadPoolName, config, computeExecutor = computeExecutor)
      .flatTap: ioRuntime =>
        val env = OurIORuntimeRegister.toEnvironment(ioRuntime)
        Js7Conf.registerInEnvironment[SyncIO](env, config)
      .allocated
      .run()

  private val actorSystemLazy = Lazy.blocking(newActorSystem(
    "JS7-Proxy",
    executionContext = ioRuntime.compute))
  private lazy val actorSystem = actorSystemLazy()
  private val _controllerApiRegister = new ControllerApiRegister(groupAndProxyId)

  given IORuntime = ioRuntime

  /// Initialisation ///
  Logger.dontInitialize()
  logger.info(StartUp.startUpLine("JS7 Proxy"))
  ProblemCodeMessages.initialize()

  // TODO Make JProxyContext a Service(?) with a JProxyContext.start or .run method
  private val stopSupportService: IO[Unit] =
    MainSupportService.service(config, useOwnResponsivenessMeter = true).allocated.map(_._2)
      .awaitInfinite

  /** Blockingly release this `JProxyContext`.*/
  def close(): Unit =
    logger.debugCall("close JS7 JProxyContext"):
      try
        releaseIO
          .logWhenItTakesLonger.awaitInfinite
      finally
        ioRuntimeShutdown.run()

  /** Asynchronously release this `JProxyContext`.*/
  private def release(): CompletableFuture[Void] =
    releaseIO
      .guarantee:
        IO:
          ioRuntimeShutdown.run()
      .as(Void).unsafeToCompletableFuture()

  private def releaseIO: IO[Unit] =
    logger.debugIO("releaseIO"):
      IO.defer:
        _singleton := None
        stopSupportService
          .guarantee:
            IO.defer:
              actorSystemLazy.toOption.foldMap:
                Pekkos.terminate

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
    CompletableFuture.supplyAsync: () =>
      new JProxyContext(groupAndProxyId.toScala, config)
    .thenCompose: jProxyContext =>
      body(jProxyContext)
        .thenCompose: result =>
          jProxyContext.release().thenApply(_ => result)
        .exceptionallyCompose: throwable =>
          jProxyContext.release().thenCompose: _ =>
            CompletableFuture.failedFuture(throwable)

  @javaApi
  def start(groupAndProxyId: Optional[GroupAndProxyId], config: Config)
  : CompletableFuture[JProxyContext] =
    CompletableFuture.supplyAsync: () =>
      new JProxyContext(groupAndProxyId.toScala, config)

  /** For Scala usage. */
  //def resourceWithOwnIORuntime(config: Config = ConfigFactory.empty)
  //: ResourceIO[JProxyContext] =
  //  for
  //    jProxyContext <-
  //  yield jProxyContext

  /** For Scala usage. */
  def resource(
    groupAndProxyId: Option[GroupAndProxyId] = None,
    config: Config = ConfigFactory.empty,
    computeExecutor: Option[Executor] = None)
  : ResourceIO[JProxyContext] =
    Resource.make(
      acquire = IO(new JProxyContext(groupAndProxyId, config, computeExecutor)))(
      release = _.releaseIO)

  def assertIsNotProxyThread(): Unit =
    assert(!currentThread.getName.startsWith(ThreadNamePrefix), "Running in a Proxy thread")

  def assertIsProxyThread(): Unit =
    val threadName = currentThread.getName
    assert(threadName.startsWith(ThreadNamePrefix), s"Not running in a Proxy thread: $threadName")
