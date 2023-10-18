package js7.data.command

import io.circe.generic.semiauto.deriveCodec
import io.circe.{Codec, Decoder, Encoder}
import js7.base.circeutils.ScalaJsonCodecs.*
import js7.base.log.CorrelId
import js7.base.utils.IntelliJUtils.intelliJuseImport
import scala.concurrent.duration.FiniteDuration

/**
 * @author Joacim Zschimmer
 */
final case class CommandRunOverview[C <: CommonCommand](
  correlId: CorrelId,
  duration: FiniteDuration,
  command: C)


object CommandRunOverview:
  intelliJuseImport(FiniteDurationJsonEncoder)

  implicit def jsonCodec[C <: CommonCommand: Encoder.AsObject: Decoder]: Codec.AsObject[CommandRunOverview[C]] =
    deriveCodec[CommandRunOverview[C]]
