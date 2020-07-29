package js7.proxy.configuration

import com.typesafe.config.Config
import js7.common.configuration.JobSchedulerConfiguration
import js7.common.configutils.Configs
import js7.common.time.JavaTimeConverters.AsScalaDuration
import js7.common.utils.JavaResource

object ProxyConfs
{
  val defaultConfig = Configs.loadResource(JavaResource("js7/proxy/configuration/proxy.conf"), internal = true)
    .withFallback(JobSchedulerConfiguration.defaultConfig)
  lazy val default = fromConfig(defaultConfig)

  def fromConfig(config: Config): ProxyConf = {
    val c = config withFallback defaultConfig
    ProxyConf(
      tornOlder = Some(c.getDuration("js7.proxy.event-stream.torn-older").toFiniteDuration))
  }
}
