package js7.common.http.configuration

import com.typesafe.config.Config
import js7.base.problem.Checked
import js7.base.time.JavaTimeConverters._

object RecouplingStreamReaderConfs
{
  def fromConfig(config: Config): Checked[RecouplingStreamReaderConf] =
    fromSubconfig(config.getConfig("js7.web.client"))

  def fromSubconfig(subconfig: Config): Checked[RecouplingStreamReaderConf] =
    Checked.catchNonFatal {
      RecouplingStreamReaderConf(
        timeout = subconfig.getDuration("idle-get-timeout").toFiniteDuration,
        delay   = subconfig.getDuration("delay-between-polling-gets").toFiniteDuration)
    }
}
