package js7.proxy.javaapi

import cats.effect.Resource
import com.typesafe.config.{Config, ConfigFactory}
import java.util.concurrent.CompletableFuture
import js7.base.annotation.javaApi
import js7.base.utils.{HasCloser, Lazy}
import js7.common.akkautils.Akkas
import js7.common.akkautils.Akkas.newActorSystem
import js7.common.log.ScribeUtils.coupleScribeWithSlf4j
import js7.common.message.ProblemCodeMessages
import js7.common.system.ThreadPools
import js7.controller.client.{AkkaHttpControllerApi, HttpControllerApi}
import js7.proxy.configuration.ProxyConfs
import js7.proxy.javaapi.JProxyContext._
import js7.proxy.javaapi.data.JHttpsConfig
import js7.proxy.javaapi.eventbus.{JControllerEventBus, JStandardEventBus}
import js7.proxy.{ControllerApi, ProxyEvent}
import monix.eval.Task
import scala.jdk.CollectionConverters._

/** The class to start. */
final class JProxyContext(config: Config)
extends HasCloser
{
  def this() = this(ConfigFactory.empty)

  coupleScribeWithSlf4j()
  ProblemCodeMessages.initialize()

  private val config_ = config
    .withFallback(ConfigFactory.systemProperties)
    .withFallback(ProxyConfs.defaultConfig)

  private val proxyConf = ProxyConfs.fromConfig(config)

  private[proxy] implicit val scheduler = ThreadPools.newStandardScheduler("JControllerProxy", config_, closer)
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
    new JControllerApi(
      apiResources,
      new ControllerApi(apiResources, proxyConf), proxyConf)
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
}

object JProxyContext
{
  private def admissionsToApiResources(
    admissions: java.lang.Iterable[JAdmission],
    httpsConfig: JHttpsConfig)
  : Seq[Resource[Task, HttpControllerApi]] =   {
    if (admissions.asScala.isEmpty) throw new IllegalArgumentException("admissions argument must not be empty")
    for ((a, i) <- admissions.asScala.map(_.underlying).zipWithIndex.toSeq)
      yield AkkaHttpControllerApi.resource(a.uri, a.userAndPassword, httpsConfig.toScala, name = s"JournaledProxy-Controller-$i")
  }
}
