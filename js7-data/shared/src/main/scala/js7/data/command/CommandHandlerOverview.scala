package js7.data.command

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

/**
 * @author Joacim Zschimmer
 */
final case class CommandHandlerOverview(
  currentCommandCount: Int,
  totalCommandCount: Long)

object CommandHandlerOverview {
  implicit val jsonCodec: Codec.AsObject[CommandHandlerOverview] = deriveCodec
}
