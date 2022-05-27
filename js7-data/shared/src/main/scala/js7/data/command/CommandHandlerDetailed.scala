package js7.data.command

import io.circe.generic.semiauto.deriveCodec
import io.circe.{Decoder, Encoder}
import js7.base.circeutils.CirceObjectCodec
/**
 * @author Joacim Zschimmer
 */
final case class CommandHandlerDetailed[C <: CommonCommand](commandRuns: Seq[CommandRunOverview[C]])

object CommandHandlerDetailed
{
  implicit def jsonCodec[C <: CommonCommand: Encoder.AsObject: Decoder]: CirceObjectCodec[CommandHandlerDetailed[C]] =
    deriveCodec[CommandHandlerDetailed[C]]
}
