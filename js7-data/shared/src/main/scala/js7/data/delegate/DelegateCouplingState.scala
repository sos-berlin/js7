package js7.data.delegate

import io.circe.generic.extras.Configuration.default.withDefaults
import js7.base.circeutils.CirceUtils.deriveConfiguredCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}

sealed trait DelegateCouplingState

object DelegateCouplingState
{
  case object Reset extends DelegateCouplingState

  case object Coupled extends DelegateCouplingState

  case object ShutDown extends DelegateCouplingState

  final case class Resetting(force: Boolean = false) extends DelegateCouplingState

  private implicit val configuration = withDefaults
  implicit val jsonCodec: TypedJsonCodec[DelegateCouplingState] = TypedJsonCodec(
    Subtype(Reset),
    Subtype(Coupled),
    Subtype(ShutDown),
    Subtype(deriveConfiguredCodec[Resetting]))
}
