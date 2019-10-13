package com.sos.jobscheduler.core.configuration

import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.common.http.configuration.RecouplingStreamReaderConf
import com.sos.jobscheduler.common.time.JavaTimeConverters._
import com.typesafe.config.Config

object RecouplingStreamReaderConfs
{
  def fromConfig(config: Config): Checked[RecouplingStreamReaderConf] =
    Checked.catchNonFatal {
      RecouplingStreamReaderConf(
        timeout = config.getDuration("jobscheduler.web.client.polling-get-without-traffic-timeout").toFiniteDuration,
        delay   = config.getDuration("jobscheduler.web.client.delay-between-polling-gets").toFiniteDuration)
    }
}
