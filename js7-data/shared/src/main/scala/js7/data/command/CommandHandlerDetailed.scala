package js7.data.command

import io.circe.generic.semiauto.deriveCodec
import io.circe.{Codec, Decoder, Encoder}
/**
 * @author Joacim Zschimmer
 */
final case class CommandHandlerDetailed[C <: CommonCommand](commandRuns: Seq[CommandRunOverview[C]])

object CommandHandlerDetailed
{
  implicit def jsonCodec[C <: CommonCommand: Encoder.AsObject: Decoder]: Codec.AsObject[CommandHandlerDetailed[C]] =
    deriveCodec[CommandHandlerDetailed[C]]
}
