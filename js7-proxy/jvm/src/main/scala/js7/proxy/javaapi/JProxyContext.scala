package js7.proxy.javaapi

import com.typesafe.config.{Config, ConfigFactory}
import java.util.concurrent.CompletableFuture
import js7.base.annotation.javaApi
import js7.base.utils.{HasCloser, Lazy}
import js7.common.akkautils.Akkas
import js7.common.akkautils.Akkas.newActorSystem
import js7.common.configuration.JobSchedulerConfiguration
import js7.common.configutils.Configs
import js7.common.log.ScribeUtils.coupleScribeWithSlf4j
import js7.common.system.ThreadPools
import js7.common.time.JavaTimeConverters.AsScalaDuration
import js7.common.utils.JavaResource
import js7.controller.client.AkkaHttpControllerApi
import js7.proxy.javaapi.JProxyContext._
import js7.proxy.javaapi.data.JHttpsConfig
import js7.proxy.{ControllerProxy, ProxyEvent}
import scala.jdk.CollectionConverters._

final class JProxyContext(config: Config)
extends HasCloser
{
  def this() = this(ConfigFactory.empty)

  private val _config = config withFallback defaultConfig
  private val tornOlder = _config.getDuration("js7.proxy.event-stream.torn-older").toFiniteDuration
  private[proxy] implicit val scheduler = ThreadPools.newStandardScheduler("JControllerProxy", _config, closer)
  private val actorSystemLazy = Lazy(newActorSystem("JS7-Proxy", defaultExecutionContext = scheduler))

  onClose {
    for (a <- actorSystemLazy) Akkas.terminateAndWait(a)
  }

  private implicit def actorSystem = actorSystemLazy()

  @javaApi
  def startControllerProxy(
    admissions: java.lang.Iterable[JAdmission],
    httpsConfig: JHttpsConfig,
    proxyEventBus: JStandardEventBus[ProxyEvent])
  : CompletableFuture[JControllerProxy] =
    startControllerProxy(admissions, httpsConfig, proxyEventBus, new JControllerEventBus)

  @javaApi
  def startControllerProxy(admissions: java.lang.Iterable[JAdmission], httpsConfig: JHttpsConfig,
    proxyEventBus: JStandardEventBus[ProxyEvent],
    controllerEventBus: JControllerEventBus)
  : CompletableFuture[JControllerProxy] = {
    val proxy = newControllerProxy(admissions, httpsConfig, proxyEventBus, controllerEventBus)
    proxy.startObserving.thenApply(_ => proxy)
  }

  @javaApi
  def newControllerProxy(admissions: java.lang.Iterable[JAdmission], httpsConfig: JHttpsConfig): JControllerProxy =
    newControllerProxy(admissions, httpsConfig, new JStandardEventBus[ProxyEvent], new JControllerEventBus)

  @javaApi
  def newControllerProxy(admissions: java.lang.Iterable[JAdmission], httpsConfig: JHttpsConfig,
    proxyEventBus: JStandardEventBus[ProxyEvent], controllerEventBus: JControllerEventBus)
  : JControllerProxy = {
    val apiResources = for ((a, i) <- admissions.asScala.map(_.underlying).zipWithIndex.toSeq)
      yield AkkaHttpControllerApi.resource(a.uri, a.userAndPassword, httpsConfig.toScala, name = s"JournaledProxy-$i")
    val proxy = ControllerProxy.apply(
      apiResources,
      proxyEventBus.underlying.publish,
      controllerEventBus.underlying.publish,
      tornOlder = Some(tornOlder))
    new JControllerProxy(proxy, proxyEventBus, controllerEventBus, this)
  }
}

object JProxyContext
{
  coupleScribeWithSlf4j()

  private val defaultConfig =
    Configs.loadResource(JavaResource("js7/proxy/configuration/proxy.conf"), internal = true)
      .withFallback(JobSchedulerConfiguration.defaultConfig)
}
