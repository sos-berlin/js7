package js7.data.command

import io.circe.{Decoder, Encoder}
import js7.base.circeutils.CirceObjectCodec
import js7.base.circeutils.CirceUtils.deriveCodec
/**
 * @author Joacim Zschimmer
 */
final case class CommandHandlerDetailed[C <: CommonCommand](commandRuns: Seq[CommandRunOverview[C]])

object CommandHandlerDetailed
{
  implicit def jsonCodec[C <: CommonCommand: Encoder.AsObject: Decoder]: CirceObjectCodec[CommandHandlerDetailed[C]] =
    deriveCodec[CommandHandlerDetailed[C]]
}
