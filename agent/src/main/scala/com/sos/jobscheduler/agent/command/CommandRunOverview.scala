package com.sos.jobscheduler.agent.command

import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.base.circeutils.JavaJsonCodecs.InstantJsonCodec
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import io.circe.generic.JsonCodec
import java.time.Instant

/**
 * @author Joacim Zschimmer
 */
@JsonCodec
final case class CommandRunOverview(
  internalId: InternalCommandId,
  startedAt: Instant,
  command: AgentCommand)

object CommandRunOverview {
  intelliJuseImport(InstantJsonCodec)
}
