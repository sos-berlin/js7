package js7.proxy.javaapi

import com.typesafe.config.{Config, ConfigFactory}
import java.util.concurrent.CompletableFuture
import js7.base.annotation.javaApi
import js7.base.utils.{Closer, HasCloser}
import js7.base.web.Uri
import js7.common.configuration.JobSchedulerConfiguration
import js7.common.configutils.Configs
import js7.common.log.ScribeUtils.coupleScribeWithSlf4j
import js7.common.system.ThreadPools
import js7.common.utils.JavaResource
import js7.controller.client.AkkaHttpControllerApi
import js7.controller.data.ControllerState
import js7.proxy.javaapi.JProxyContext._
import js7.proxy.javaapi.data.JHttpsConfig
import js7.proxy.{JournaledProxy, ProxyEvent}
import monix.execution.FutureUtils.Java8Extensions

final class JProxyContext(config: Config)
extends HasCloser
{
  def this() = this(ConfigFactory.empty)

  private val _config = config withFallback defaultConfig
  private[proxy] implicit val scheduler = ThreadPools.newStandardScheduler("JControllerProxy", _config, closer)

  @javaApi
  def startControllerProxy(uri: String, credentials: JCredentials, httpsConfig: JHttpsConfig, proxyEventBus: JStandardEventBus[ProxyEvent])
  : CompletableFuture[JControllerProxy] =
    startControllerProxy(uri, credentials, httpsConfig, proxyEventBus, new JControllerEventBus)

  @javaApi
  def startControllerProxy(uri: String, credentials: JCredentials, httpsConfig: JHttpsConfig,
    proxyEventBus: JStandardEventBus[ProxyEvent],
    controllerEventBus: JControllerEventBus)
  : CompletableFuture[JControllerProxy] = {
    val apiResource = AkkaHttpControllerApi.separateAkkaResource(Uri(uri), credentials.toUnderlying, httpsConfig.toScala, _config)

    JournaledProxy.start[ControllerState](apiResource, proxyEventBus.underlying.publish, controllerEventBus.underlying.publish)
      .map(proxy => new JControllerProxy(proxy, proxyEventBus, controllerEventBus, apiResource, this))
      .runToFuture
      .asJava
  }
}

object JProxyContext
{
  coupleScribeWithSlf4j()

  private val defaultConfig =
    Configs.loadResource(JavaResource("js7/proxy/configuration/proxy.conf"), internal = true)
      .withFallback(JobSchedulerConfiguration.defaultConfig)
}
