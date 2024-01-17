package js7.common.http.configuration

import com.typesafe.config.{Config, ConfigException}
import js7.base.problem.Checked
import js7.base.problem.Checked.catchExpected
import js7.base.time.JavaTimeConverters.*
import js7.base.time.ScalaTime.DurationRichInt
import js7.base.utils.CatsUtils.Nel
import scala.jdk.CollectionConverters.*

object RecouplingStreamReaderConfs:
  def fromConfig(config: Config): Checked[RecouplingStreamReaderConf] =
    catchExpected[ConfigException]:
      RecouplingStreamReaderConf(
        timeout = config.getDuration("js7.web.client.idle-get-timeout").toFiniteDuration,
        delay   = config.getDuration("js7.web.client.polling-delay").toFiniteDuration,
        failureDelays = Nel
          .fromList(config
            .getDurationList("js7.web.client.failure-delays").asScala
            .map(_.toFiniteDuration).toList)
          .getOrElse(Nel.one(10.s)))
