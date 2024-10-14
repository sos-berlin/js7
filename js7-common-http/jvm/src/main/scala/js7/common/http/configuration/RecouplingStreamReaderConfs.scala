package js7.common.http.configuration

import com.typesafe.config.{Config, ConfigException}
import js7.base.configutils.Configs.RichConfig
import js7.base.problem.Checked
import js7.base.problem.Checked.catchExpected
import js7.base.time.JavaTimeConverters.*
import js7.base.time.ScalaTime.DurationRichInt
import js7.base.utils.CatsUtils.Nel
import js7.base.utils.DelayConf
import scala.jdk.CollectionConverters.*

object RecouplingStreamReaderConfs:

  def fromConfig(config: Config): Checked[RecouplingStreamReaderConf] =
    for
      timeout <- config.finiteDuration("js7.web.client.idle-get-timeout")
      keepAlive <- config.finiteDuration("js7.web.client.keep-alive")
      delay <- config.finiteDuration("js7.web.client.polling-delay")
      result <- catchExpected[ConfigException]:
        RecouplingStreamReaderConf(
          timeout = Some(timeout),
          keepAlive = keepAlive,
          delay = delay,
          delayConf = DelayConf:
            Nel.fromList:
              config.getDurationList("js7.web.client.failure-delays").asScala.toList
                .map(_.toFiniteDuration)
            .getOrElse:
              Nel.one(10.s))
    yield
      result
