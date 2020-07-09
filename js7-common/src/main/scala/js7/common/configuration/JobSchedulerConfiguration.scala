package js7.common.configuration

import com.typesafe.config.Config
import js7.common.configutils.Configs
import js7.common.utils.JavaResource

object JobSchedulerConfiguration
{
  val defaultConfig: Config =
    Configs.loadResource(
      JavaResource("js7/common/configuration/jobscheduler.conf"),
      internal = true)
}
