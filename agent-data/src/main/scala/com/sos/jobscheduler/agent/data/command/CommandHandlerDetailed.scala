package com.sos.jobscheduler.agent.data.command

import com.sos.jobscheduler.common.scalautil.Logger
import io.circe.generic.JsonCodec
import scala.collection.immutable.Seq

/**
 * @author Joacim Zschimmer
 */
@JsonCodec
final case class CommandHandlerDetailed(commandRuns: Seq[CommandRunOverview])

object CommandHandlerDetailed {
  private val logger = Logger(getClass)
}
