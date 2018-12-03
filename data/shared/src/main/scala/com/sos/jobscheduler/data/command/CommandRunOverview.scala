package com.sos.jobscheduler.data.command

import com.sos.jobscheduler.base.circeutils.CirceObjectCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.time.Timestamp
import io.circe.{Decoder, ObjectEncoder}

/**
 * @author Joacim Zschimmer
 */
final case class CommandRunOverview[C <: CommonCommand](
  internalId: InternalCommandId,
  startedAt: Timestamp,
  command: C)

object CommandRunOverview
{
  implicit def jsonCodec[C <: CommonCommand: ObjectEncoder: Decoder]: CirceObjectCodec[CommandRunOverview[C]] =
    deriveCodec[CommandRunOverview[C]]
}
