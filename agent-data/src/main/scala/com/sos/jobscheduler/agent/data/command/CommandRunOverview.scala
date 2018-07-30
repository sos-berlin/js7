package com.sos.jobscheduler.agent.data.command

import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.JavaJsonCodecs.instant.StringInstantJsonCodec
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import java.time.Instant

/**
 * @author Joacim Zschimmer
 */
final case class CommandRunOverview(
  internalId: InternalCommandId,
  startedAt: Instant,
  command: AgentCommand)

object CommandRunOverview {
  intelliJuseImport(StringInstantJsonCodec)
  implicit val jsonCodec = deriveCodec[CommandRunOverview]
}
