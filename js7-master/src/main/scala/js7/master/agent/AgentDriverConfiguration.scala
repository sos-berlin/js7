package js7.master.agent

import js7.base.problem.Checked
import js7.common.http.configuration.{RecouplingStreamReaderConf, RecouplingStreamReaderConfs}
import js7.common.time.JavaTimeConverters._
import js7.core.event.journal.JournalConf
import com.typesafe.config.Config
import scala.concurrent.duration._

final case class AgentDriverConfiguration(
  recouplingStreamReader: RecouplingStreamReaderConf,
  eventBufferDelay: FiniteDuration,
  eventBufferSize: Int,
  commitDelay: FiniteDuration,
  commandBatchSize: Int,
  commandBatchDelay: FiniteDuration,
  commandErrorDelay: FiniteDuration,
  commandParallelism: Int,
  releaseEventsPeriod: FiniteDuration)

object AgentDriverConfiguration
{
  def fromConfig(config: Config, journalConf: JournalConf): Checked[AgentDriverConfiguration] =
    RecouplingStreamReaderConfs.fromConfig(config)
      .flatMap(recouplingStreamReader =>
        Checked.catchNonFatal {
          new AgentDriverConfiguration(
            recouplingStreamReader,
            eventBufferDelay    = config.getDuration("js7.master.agent-driver.event-buffer-delay").toFiniteDuration,
            eventBufferSize     = config.getInt     ("js7.master.agent-driver.event-buffer-size"),
            commitDelay         = journalConf.delay,
            commandBatchSize    = config.getInt     ("js7.master.agent-driver.command-batch-size"),
            commandBatchDelay   = config.getDuration("js7.master.agent-driver.command-batch-delay").toFiniteDuration,
            commandErrorDelay   = config.getDuration("js7.master.agent-driver.command-error-delay").toFiniteDuration,
            commandParallelism  = config.getInt     ("js7.master.agent-driver.command-parallelism"),
            releaseEventsPeriod    = config.getDuration("js7.master.agent-driver.release-events-period").toFiniteDuration)
        })
}
