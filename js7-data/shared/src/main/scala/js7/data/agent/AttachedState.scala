package js7.data.agent

import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}

sealed trait AttachedState

object AttachedState
{
  case object Attaching extends AttachedState

  case object Attached extends AttachedState

  implicit val jsonCodec = TypedJsonCodec[AttachedState](
    Subtype(Attaching),
    Subtype(Attached))
}
