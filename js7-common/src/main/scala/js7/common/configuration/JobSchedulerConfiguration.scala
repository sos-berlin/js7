package js7.common.configuration

import js7.common.configutils.Configs
import js7.common.utils.JavaResource

object JobSchedulerConfiguration
{
  lazy val defaultConfig = Configs.loadResource(
      JavaResource("js7/common/configuration/jobscheduler.conf"),
      internal = true)
}
