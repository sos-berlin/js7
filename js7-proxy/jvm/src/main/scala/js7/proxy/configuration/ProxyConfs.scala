package js7.proxy.configuration

import com.typesafe.config.Config
import js7.base.config.Js7Config
import js7.base.configutils.Configs
import js7.base.io.JavaResource
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.common.http.configuration.RecouplingStreamReaderConfs

object ProxyConfs:
  val defaultConfig: Config = Configs.loadResource(JavaResource("js7/proxy/configuration/proxy.conf"))
    .withFallback(Js7Config.defaultConfig)
  lazy val default: ProxyConf = fromConfig(defaultConfig)

  def fromConfig(config: Config): ProxyConf =
    val c = config.withFallback(defaultConfig)
    ProxyConf(
      tornOlder = Some(c.getDuration("js7.proxy.torn-older").toFiniteDuration),
      syncPolling = c.getDuration("js7.proxy.sync-polling").toFiniteDuration,
      eventQueueSize = c.getInt("js7.proxy.event-queue-size"),
      RecouplingStreamReaderConfs.fromConfig(config).orThrow)
