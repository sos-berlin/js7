package js7.data.fatevent

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.data.event.NoKeyEvent
import js7.data.master.MasterId

/**
  * @author Joacim Zschimmer
  */
sealed trait MasterFatEvent extends FatEvent with NoKeyEvent

object MasterFatEvent
{
  final case class MasterReadyFat(masterId: MasterId, timezone: String) extends MasterFatEvent

  implicit val jsonCodec: TypedJsonCodec[MasterFatEvent] = TypedJsonCodec(
    Subtype(deriveCodec[MasterReadyFat]))
}
