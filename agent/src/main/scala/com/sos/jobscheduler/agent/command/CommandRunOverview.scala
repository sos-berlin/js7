package com.sos.jobscheduler.agent.command

import com.sos.jobscheduler.agent.data.commands.AgentCommand
import io.circe.generic.JsonCodec
import java.time.Instant
import com.sos.jobscheduler.base.circeutils.JavaJsonCodecs.InstantJsonCodec

/**
 * @author Joacim Zschimmer
 */
@JsonCodec
final case class CommandRunOverview(
  internalId: InternalCommandId,
  startedAt: Instant,
  command: AgentCommand)
