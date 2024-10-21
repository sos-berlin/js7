package js7.base.utils

import com.typesafe.config.Config
import js7.base.configutils.Configs.RichConfig
import js7.base.problem.Checked

object DelayConfs:

  def fromConfig(config: Config, key: String): Checked[DelayConf] =
    config.nonEmptyFiniteDurations(key).map:
      DelayConf(_)
