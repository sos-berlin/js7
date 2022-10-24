package js7.common.http.configuration

import com.typesafe.config.{Config, ConfigException}
import js7.base.problem.Checked
import js7.base.problem.Checked.catchExpected
import js7.base.time.JavaTimeConverters.*

object RecouplingStreamReaderConfs
{
  def fromConfig(config: Config): Checked[RecouplingStreamReaderConf] =
    catchExpected[ConfigException] {
      RecouplingStreamReaderConf(
        timeout = config.getDuration("js7.web.client.idle-get-timeout").toFiniteDuration,
        delay   = config.getDuration("js7.web.client.polling-delay").toFiniteDuration,
        failureDelay = config.getDuration("js7.web.client.failure-delay").toFiniteDuration)
    }
}
