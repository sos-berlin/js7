package js7.data.command

import js7.base.circeutils.CirceObjectCodec
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.ScalaJsonCodecs._
import js7.base.utils.IntelliJUtils.intelliJuseImport
import io.circe.{Decoder, Encoder}
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

  implicit def jsonCodec[C <: CommonCommand: Encoder.AsObject: Decoder]: CirceObjectCodec[CommandRunOverview[C]] =
    deriveCodec[CommandRunOverview[C]]
}
