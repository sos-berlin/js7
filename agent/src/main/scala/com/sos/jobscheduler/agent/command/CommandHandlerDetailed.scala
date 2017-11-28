package com.sos.jobscheduler.agent.command

import io.circe.generic.JsonCodec
import scala.collection.immutable.Seq

/**
 * @author Joacim Zschimmer
 */
@JsonCodec
final case class CommandHandlerDetailed(commandRuns: Seq[CommandRunOverview])
