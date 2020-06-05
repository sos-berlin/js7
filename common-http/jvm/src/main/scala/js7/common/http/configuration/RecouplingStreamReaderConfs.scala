package js7.common.http.configuration

import js7.base.problem.Checked
import js7.common.time.JavaTimeConverters._
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
