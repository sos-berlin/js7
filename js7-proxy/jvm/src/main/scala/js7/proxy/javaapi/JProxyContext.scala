package js7.proxy.javaapi

import cats.effect.SyncIO
import cats.effect.unsafe.IORuntime
import com.typesafe.config.{Config, ConfigFactory}
import java.util.concurrent.Executor
import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.base.catsutils.OurIORuntime
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.system.startup.StartUp
import js7.base.utils.CatsUtils.*
import js7.base.utils.Lazy
import js7.common.message.ProblemCodeMessages
import js7.common.pekkoutils.Pekkos
import js7.common.pekkoutils.Pekkos.newActorSystem
import js7.controller.client.PekkoHttpControllerApi.admissionsToApiResource
import js7.data_for_java.auth.{JAdmission, JHttpsConfig}
import js7.proxy.ControllerApi
import js7.proxy.configuration.ProxyConfs
import js7.proxy.javaapi.JProxyContext.*
import scala.jdk.CollectionConverters.*

/** The class to start. */
final class JProxyContext(config: Config, computeExecutor: Executor | Null)
extends AutoCloseable:
  def this() = this(ConfigFactory.empty, null)
  def this(config: Config) = this(config, null)

  Logger.dontInitialize()
  logger.info(StartUp.startUpLine("JS7 Proxy"))
  ProblemCodeMessages.initialize()

  private val config_ = config
    .withFallback(ConfigFactory.systemProperties)
    .withFallback(ProxyConfs.defaultConfig)

  private val proxyConf = ProxyConfs.fromConfig(config_)

  private val (ioRuntime, ioRuntimeShutdown) =
    OurIORuntime
      .resource[SyncIO]("JS7 Proxy", config_, computeExecutor = Option(computeExecutor))
      .allocated
      .unsafeRunSync()

  private given IORuntime = ioRuntime

  private val actorSystemLazy = Lazy(newActorSystem(
    "JS7-Proxy",
    executionContext = ioRuntime.compute))
  private lazy val actorSystem = actorSystemLazy()

  def close(): Unit =
    logger.debugCall("close JS7 JProxyContext"):
      try
        for a <- actorSystemLazy do Pekkos.terminateAndWait(a)
      finally
        ioRuntimeShutdown.unsafeRunSync()

  @javaApi @Nonnull
  def newControllerApi(
    @Nonnull admissions: java.lang.Iterable[JAdmission],
    @Nonnull httpsConfig: JHttpsConfig)
  : JControllerApi =
    val apiResource = admissionsToApiResource(
      Nel.unsafe(admissions.asScala.map(_.asScala).toList),
      httpsConfig.asScala)(
      actorSystem)
    new JControllerApi(
      new ControllerApi(apiResource, proxyConf),
      config_)


object JProxyContext:

  Logger.dontInitialize()
  private lazy val logger = Logger[this.type]
