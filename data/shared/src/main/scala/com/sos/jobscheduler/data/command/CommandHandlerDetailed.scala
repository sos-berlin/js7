package com.sos.jobscheduler.data.command

import com.sos.jobscheduler.base.circeutils.CirceObjectCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import io.circe.{Decoder, ObjectEncoder}
import scala.collection.immutable.Seq

/**
 * @author Joacim Zschimmer
 */
final case class CommandHandlerDetailed[C <: CommonCommand](commandRuns: Seq[CommandRunOverview[C]])

object CommandHandlerDetailed
{
  implicit def jsonCodec[C <: CommonCommand: ObjectEncoder: Decoder]: CirceObjectCodec[CommandHandlerDetailed[C]] =
    deriveCodec[CommandHandlerDetailed[C]]
}
