package com.sos.jobscheduler.data.command

import com.sos.jobscheduler.base.circeutils.CirceObjectCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import io.circe.{Decoder, Encoder}
import scala.collection.immutable.Seq

/**
 * @author Joacim Zschimmer
 */
final case class CommandHandlerDetailed[C <: CommonCommand](commandRuns: Seq[CommandRunOverview[C]])

object CommandHandlerDetailed
{
  implicit def jsonCodec[C <: CommonCommand: Encoder.AsObject: Decoder]: CirceObjectCodec[CommandHandlerDetailed[C]] =
    deriveCodec[CommandHandlerDetailed[C]]
}
