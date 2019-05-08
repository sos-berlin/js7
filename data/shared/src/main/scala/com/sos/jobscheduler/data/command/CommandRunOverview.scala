package com.sos.jobscheduler.data.command

import com.sos.jobscheduler.base.circeutils.CirceObjectCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.ScalaJsonCodecs._
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import io.circe.{Decoder, ObjectEncoder}
import scala.concurrent.duration.FiniteDuration

/**
 * @author Joacim Zschimmer
 */
final case class CommandRunOverview[C <: CommonCommand](
  internalId: InternalCommandId,
  duration: FiniteDuration,
  command: C)

object CommandRunOverview
{
  intelliJuseImport(FiniteDurationJsonEncoder)

  implicit def jsonCodec[C <: CommonCommand: ObjectEncoder: Decoder]: CirceObjectCodec[CommandRunOverview[C]] =
    deriveCodec[CommandRunOverview[C]]
}
