package js7.subagent.configuration

import com.typesafe.config.Config
import js7.base.configutils.Configs.RichConfig
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.utils.ScalaUtils.syntax.*
import scala.concurrent.duration.FiniteDuration

final case class StdouterrConf(chunkSize: Int, delay: FiniteDuration)

object StdouterrConf:
  def fromConfig(config: Config): StdouterrConf = new StdouterrConf(
    chunkSize = config.memorySizeAsInt("js7.order.stdout-stderr.chunk-size").orThrow,
    delay = config.getDuration("js7.order.stdout-stderr.delay").toFiniteDuration)
