package js7.controller.agent

import com.typesafe.config.Config
import js7.base.problem.Checked
import js7.base.time.JavaTimeConverters.*
import js7.common.http.configuration.{RecouplingStreamReaderConf, RecouplingStreamReaderConfs}
import js7.journal.configuration.JournalConf
import scala.concurrent.duration.*

final case class AgentDriverConfiguration(
  recouplingStreamReader: RecouplingStreamReaderConf,
  eventBufferDelay: FiniteDuration,
  eventBufferSize: Int,
  commitDelay: FiniteDuration,
  commandBatchSize: Int,
  commandBatchDelay: FiniteDuration,
  commandErrorDelay: FiniteDuration,
  commandParallelism: Int,
  releaseEventsPeriod: FiniteDuration,
  config: Config)


object AgentDriverConfiguration:
  def fromConfig(config: Config, journalConf: JournalConf): Checked[AgentDriverConfiguration] =
    RecouplingStreamReaderConfs.fromConfig(config)
      .flatMap(recouplingStreamReader =>
        Checked.catchNonFatal {
          new AgentDriverConfiguration(
            recouplingStreamReader,
            eventBufferDelay    = config.getDuration("js7.controller.agent-driver.event-buffer-delay").toFiniteDuration,
            eventBufferSize     = config.getInt     ("js7.controller.agent-driver.event-buffer-size"),
            commitDelay         = journalConf.delay,
            commandBatchSize    = config.getInt     ("js7.controller.agent-driver.command-batch-size"),
            commandBatchDelay   = config.getDuration("js7.controller.agent-driver.command-batch-delay").toFiniteDuration,
            commandErrorDelay   = config.getDuration("js7.controller.agent-driver.command-error-delay").toFiniteDuration,
            commandParallelism  = config.getInt     ("js7.controller.agent-driver.command-parallelism"),
            releaseEventsPeriod = config.getDuration("js7.controller.agent-driver.release-events-period").toFiniteDuration,
            config)
        })
