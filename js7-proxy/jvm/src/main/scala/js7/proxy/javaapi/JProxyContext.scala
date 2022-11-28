package js7.proxy.javaapi

import com.typesafe.config.{Config, ConfigFactory}
import java.util.concurrent.ForkJoinPool
import javax.annotation.Nonnull
import js7.base.BuildInfo
import js7.base.annotation.javaApi
import js7.base.log.ScribeForJava.coupleScribeWithSlf4j
import js7.base.log.{CorrelId, Logger}
import js7.base.utils.CatsUtils.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{HasCloser, Lazy}
import js7.common.akkautils.Akkas
import js7.common.akkautils.Akkas.newActorSystem
import js7.common.message.ProblemCodeMessages
import js7.common.system.ThreadPools
import js7.common.system.startup.StartUp
import js7.controller.client.AkkaHttpControllerApi.admissionsToApiResources
import js7.data_for_java.auth.{JAdmission, JHttpsConfig}
import js7.proxy.ControllerApi
import js7.proxy.configuration.ProxyConfs
import js7.proxy.javaapi.JProxyContext.*
import monix.execution.Scheduler
import scala.jdk.CollectionConverters.*

/** The class to start. */
final class JProxyContext(config: Config)
extends HasCloser
{
  def this() = this(ConfigFactory.empty)

  // Log early for early timestamp and proper logger initialization by a single (not-parallel) call
  logger.info(s"JS7 Proxy ${BuildInfo.longVersion}")
  logger.debug(StartUp.startUpLine())

  // Do not initialize Log4j and log4j2.threadContextMap.
  // It's the caller responsibility. It is to late, anyway.
  // No CorrelIds are logged.
  coupleScribeWithSlf4j(noLog4jInit = true)

  ProblemCodeMessages.initialize()

  private val config_ = config
    .withFallback(ConfigFactory.systemProperties)
    .withFallback(ProxyConfs.defaultConfig)

  private val proxyConf = ProxyConfs.fromConfig(config_)

  private val useJavaThreadPool = config_.getBoolean("js7.thread-pools.use-java-thread-pool")
  private val ownScheduler = !useJavaThreadPool ?
    ThreadPools.newStandardScheduler("JControllerProxy", config_, closer)
  private[proxy] implicit val scheduler: Scheduler =
    CorrelId.enableScheduler(
      ownScheduler getOrElse Scheduler(ForkJoinPool.commonPool))

  private val actorSystemLazy = Lazy(newActorSystem(
    "JS7-Proxy",
    defaultExecutionContext = scheduler))
  private lazy val actorSystem = actorSystemLazy()

  onClose {
    logger.debug("close JS7 JProxyContext")
    for (a <- actorSystemLazy) Akkas.terminateAndWait(a)
  }

  @javaApi @Nonnull
  def newControllerApi(
    @Nonnull admissions: java.lang.Iterable[JAdmission],
    @Nonnull httpsConfig: JHttpsConfig
  ): JControllerApi = {
    val apiResources = admissionsToApiResources(
      Nel.unsafe(admissions.asScala.map(_.asScala).toList),
      httpsConfig.asScala)(
      actorSystem)
    new JControllerApi(
      new ControllerApi(apiResources, proxyConf))
  }
}

object JProxyContext
{
  private val logger = Logger(getClass)
}
