package com.sos.jobscheduler.agent.command

import io.circe.generic.JsonCodec

/**
 * @author Joacim Zschimmer
 */
@JsonCodec
final case class CommandHandlerOverview(
  currentCommandCount: Int,
  totalCommandCount: Long)
