package js7.data.lock

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.data.event.Event

sealed trait LockEvent extends Event {
  type Key = LockId
}

object LockEvent {

  final case class LockAdded(limit: Int)
  extends LockEvent

  final case class LockUpdated(limit: Int)
  extends LockEvent

  implicit val jsonCodec = TypedJsonCodec[LockEvent](
    Subtype(deriveCodec[LockAdded]),
    Subtype(deriveCodec[LockUpdated]))
}
