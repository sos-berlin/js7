package js7.proxy.javaapi

import cats.effect.Resource
import com.typesafe.config.{Config, ConfigFactory}
import java.util.concurrent.{CompletableFuture, ForkJoinPool}
import js7.base.BuildInfo
import js7.base.annotation.javaApi
import js7.base.utils.{HasCloser, Lazy}
import js7.common.akkautils.Akkas
import js7.common.akkautils.Akkas.newActorSystem
import js7.common.log.ScribeUtils.coupleScribeWithSlf4j
import js7.common.message.ProblemCodeMessages
import js7.common.scalautil.Logger
import js7.common.system.ThreadPools
import js7.common.system.startup.StartUp
import js7.controller.client.{AkkaHttpControllerApi, HttpControllerApi}
import js7.proxy.configuration.ProxyConfs
import js7.proxy.data.ProxyEvent
import js7.proxy.javaapi.JProxyContext._
import js7.proxy.javaapi.data.auth.{JAdmission, JHttpsConfig}
import js7.proxy.javaapi.eventbus.{JControllerEventBus, JStandardEventBus}
import monix.eval.Task
import monix.execution.Scheduler
import scala.jdk.CollectionConverters._

/** The class to start. */
final class JProxyContext(config: Config)
extends HasCloser
{
  def this() = this(ConfigFactory.empty)

  // Log early for early timestamp and proper logger initialization by a single (not-parallel) call
  logger.debug(s"JS7 JobScheduler Proxy ${BuildInfo.prettyVersion}")
  logger.debug(StartUp.startUpLine())

  coupleScribeWithSlf4j()
  ProblemCodeMessages.initialize()

  private val config_ = config
    .withFallback(ConfigFactory.systemProperties)
    .withFallback(ProxyConfs.defaultConfig)

  private val proxyConf = ProxyConfs.fromConfig(config)

  private val useJavaThreadPool = config_.getBoolean("js7.thread-pools.use-java-thread-pool")
  private[proxy] implicit val scheduler =
    if (useJavaThreadPool)
      ThreadPools.newStandardScheduler("JControllerProxy", config_, closer)
    else
      Scheduler(ForkJoinPool.commonPool)
  private val actorSystemLazy = Lazy(newActorSystem("JS7-Proxy", defaultExecutionContext = scheduler))

  onClose {
    for (a <- actorSystemLazy) Akkas.terminateAndWait(a)
  }

  private implicit def actorSystem = actorSystemLazy()

  @javaApi
  def newControllerApi(
    admissions: java.lang.Iterable[JAdmission],
    httpsConfig: JHttpsConfig)
  : JControllerApi = {
    val apiResources = admissionsToApiResources(admissions, httpsConfig)
    new JControllerApi(apiResources, proxyConf)
  }

  /** Convenience method, starts a `JControllerProxy`.
    * After use, stop it with `JControllerProxy.stop()`. */
  def startProxy(
    admissions: java.lang.Iterable[JAdmission],
    httpsConfig: JHttpsConfig,
    proxyEventBus: JStandardEventBus[ProxyEvent],
    controllerEventBus: JControllerEventBus)
  : CompletableFuture[JControllerProxy] =
    newControllerApi(admissions, httpsConfig)
      .startProxy(proxyEventBus, controllerEventBus)

  private def admissionsToApiResources(
    admissions: java.lang.Iterable[JAdmission],
    httpsConfig: JHttpsConfig)
  : Seq[Resource[Task, HttpControllerApi]] =   {
    if (admissions.asScala.isEmpty) throw new IllegalArgumentException("admissions argument must not be empty")
    for ((a, i) <- admissions.asScala.map(_.asScala).zipWithIndex.toSeq)
      yield AkkaHttpControllerApi.resource(a.uri, a.userAndPassword, httpsConfig.asScala, name = s"JournaledProxy-Controller-$i")
  }
}

object JProxyContext
{
  private val logger = Logger(getClass)
}
