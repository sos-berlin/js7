package com.sos.jobscheduler.core.configuration

import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.common.http.configuration.RecouplingStreamReaderConf
import com.sos.jobscheduler.common.time.JavaTimeConverters._
import com.typesafe.config.Config

object RecouplingStreamReaderConfs
{
  def fromConfig(config: Config): Checked[RecouplingStreamReaderConf] =
    fromSubconfig(config.getConfig("jobscheduler.web.client"))

  def fromSubconfig(subconfig: Config): Checked[RecouplingStreamReaderConf] =
    Checked.catchNonFatal {
      RecouplingStreamReaderConf(
        timeout = subconfig.getDuration("idle-get-timeout").toFiniteDuration,
        delay   = subconfig.getDuration("delay-between-polling-gets").toFiniteDuration)
    }
}
