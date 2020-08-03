package js7.proxy.javaapi

import com.typesafe.config.{Config, ConfigFactory}
import js7.base.annotation.javaApi
import js7.base.utils.{HasCloser, Lazy}
import js7.common.akkautils.Akkas
import js7.common.akkautils.Akkas.newActorSystem
import js7.common.log.ScribeUtils.coupleScribeWithSlf4j
import js7.common.message.ProblemCodeMessages
import js7.common.system.ThreadPools
import js7.controller.client.AkkaHttpControllerApi
import js7.proxy.ControllerApi
import js7.proxy.configuration.ProxyConfs
import js7.proxy.javaapi.data.JHttpsConfig
import scala.jdk.CollectionConverters._

final class JProxyContext(config: Config)
extends HasCloser
{
  def this() = this(ConfigFactory.empty)

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
  def newControllerApi(admissions: java.lang.Iterable[JAdmission], httpsConfig: JHttpsConfig): JControllerApi = {
    if (admissions.asScala.isEmpty) throw new IllegalArgumentException("admissions argument must not be empty")
    val apiResources = for ((a, i) <- admissions.asScala.map(_.underlying).zipWithIndex.toSeq)
      yield AkkaHttpControllerApi.resource(a.uri, a.userAndPassword, httpsConfig.toScala, name = s"JournaledProxy-$i")
    new JControllerApi(apiResources, new ControllerApi(apiResources, proxyConf), proxyConf)
  }
}

object JProxyContext
{
  coupleScribeWithSlf4j()
  ProblemCodeMessages.initialize()
}
