package js7.proxy.javaapi

import cats.effect.unsafe.IORuntime
import cats.effect.{IO, Resource, ResourceIO, SyncIO}
import cats.syntax.flatMap.*
import com.typesafe.config.{Config, ConfigFactory}
import java.lang.Thread.currentThread
import java.util.concurrent.{CompletableFuture, Executor}
import java.util.function.Supplier
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
import js7.base.utils.CatsUtils.*
import js7.base.utils.Lazy
import js7.base.utils.Nulls.nullToNone
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.message.ProblemCodeMessages
import js7.common.pekkoutils.Pekkos
import js7.common.pekkoutils.Pekkos.newActorSystem
import js7.controller.client.PekkoHttpControllerApi.admissionsToApiResource
import js7.data_for_java.auth.{JAdmission, JHttpsConfig}
import js7.data_for_java.common.JavaUtils.{-->, Void}
import js7.proxy.ControllerApi
import js7.proxy.configuration.ProxyConfs
import js7.proxy.javaapi.JProxyContext.*
import scala.annotation.static
import scala.jdk.CollectionConverters.*

/** The class to start. */
final class JProxyContext(config_ : Config, computeExecutor: Executor | Null)
extends AutoCloseable:

  def this() = this(ConfigFactory.empty, null)
  def this(config: Config) = this(config, null)

  Logger.dontInitialize()
  logger.info(StartUp.startUpLine("JS7 Proxy"))
  ProblemCodeMessages.initialize()

  val config: Config =
    config_
      .withFallback(ConfigFactory.systemProperties)
      .withFallback(ProxyConfs.defaultConfig)

  private val proxyConf = ProxyConfs.fromConfig(config)

  private val (ioRuntime, ioRuntimeShutdown) =
    OurIORuntime.resource[SyncIO](ThreadPoolName, config, computeExecutor = nullToNone(computeExecutor))
      .flatTap: ioRuntime =>
        val env = OurIORuntimeRegister.toEnvironment(ioRuntime)
        Js7Conf.registerInEnvironment[SyncIO](env, config)
      .allocated
      .run()

  given IORuntime = ioRuntime

  private val actorSystemLazy = Lazy.blocking(newActorSystem(
    "JS7-Proxy",
    executionContext = ioRuntime.compute))
  private lazy val actorSystem = actorSystemLazy()

  /** Blockingly release this `JProxyContext`.*/
  def close(): Unit =
    logger.debugCall("close JS7 JProxyContext"):
      try
        for a <- actorSystemLazy do Pekkos.terminateAndWait(a)
      finally
        ioRuntimeShutdown.run()

  /** Asynchronously release this `JProxyContext`.*/
  def release(): CompletableFuture[Void] =
    releaseIO
      .as(Void).unsafeToCompletableFuture()

  private def releaseIO: IO[Unit] =
    logger.debugIO("releaseIO"):
      actorSystemLazy.toOption.foldMap:
        Pekkos.terminate
      .guarantee:
        IO:
          ioRuntimeShutdown.run()

  /** Runs `body` with an own [[JControllerApi]].
    */
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


  @javaApi @Nonnull
  def newControllerApi(
    @Nonnull admissions: java.lang.Iterable[JAdmission],
    @Nonnull httpsConfig: JHttpsConfig)
  : JControllerApi =
    val apiResource = admissionsToApiResource(
      admissions = Nel.unsafe(admissions.asScala.map(_.asScala).toList),
      httpsConfig.asScala
    )(using actorSystem)
    new JControllerApi(
      new ControllerApi(apiResource, proxyConf),
      config)

  /** For Scala usage. */
  def controllerApiResource(
    admissions: Nel[Admission],
    httpsConfig: HttpsConfig = HttpsConfig.empty)
  : ResourceIO[JControllerApi] =
    Resource.make(
      acquire = IO(newControllerApi(admissions.toList.map(JAdmission(_)).asJava, JHttpsConfig(httpsConfig))))(
      release = api =>
        IO.fromCompletableFuture:
          IO:
            api.stop()
        .void)


object JProxyContext:

  Logger.dontInitialize()
  @static private val ThreadPoolName = "JS7-Proxy"
  @static val ThreadNamePrefix: String = s"$ThreadPoolName-"
  private lazy val logger = Logger[this.type]

  /** Runs `body` with an own [[JProxyContext]]. */
  def run[A](config: Config, body: JProxyContext --> CompletableFuture[A]): CompletableFuture[A] =
    CompletableFuture.supplyAsync: () =>
      new JProxyContext()
    .thenCompose: jProxyContext =>
      body(jProxyContext)
        .thenCompose: result =>
          jProxyContext.release().thenApply(_ => result)
        .exceptionallyCompose: throwable =>
          jProxyContext.release().thenCompose: _ =>
            CompletableFuture.failedFuture(throwable)
  /** For Scala usage. */
  //def resourceWithOwnIORuntime(config: Config = ConfigFactory.empty)
  //: ResourceIO[JProxyContext] =
  //  for
  //    jProxyContext <-
  //  yield jProxyContext

  /** For Scala usage. */
  def resource(
    config: Config = ConfigFactory.empty,
    computeExecutor: Executor | Null = null)
  : ResourceIO[JProxyContext] =
    Resource.make(
      acquire = IO(new JProxyContext(config, computeExecutor)))(
      release = _.releaseIO)

  def assertIsNotProxyThread(): Unit =
    assert(!currentThread.getName.startsWith(ThreadNamePrefix), "Running in a Proxy thread")

  def assertIsProxyThread(): Unit =
    val threadName = currentThread.getName
    assert(threadName.startsWith(ThreadNamePrefix), s"Not running in a Proxy thread: $threadName")
