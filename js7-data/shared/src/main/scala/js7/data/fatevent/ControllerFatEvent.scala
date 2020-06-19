package js7.data.fatevent

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.data.controller.ControllerId
import js7.data.event.NoKeyEvent

/**
  * @author Joacim Zschimmer
  */
sealed trait ControllerFatEvent extends FatEvent with NoKeyEvent

object ControllerFatEvent
{
  final case class ControllerReadyFat(controllerId: ControllerId, timezone: String) extends ControllerFatEvent

  implicit val jsonCodec: TypedJsonCodec[ControllerFatEvent] = TypedJsonCodec(
    Subtype(deriveCodec[ControllerReadyFat]))
}
