package js7.data.lock

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.data.event.Event

sealed trait LockEvent extends Event {
  type Key = LockName
}

object LockEvent {

  final case class LockAdded(nonExclusiveLimit: Option[Int])
  extends LockEvent

  final case class LockUpdated(nonExclusiveLimit: Option[Int])
  extends LockEvent

  implicit val jsonCodec = TypedJsonCodec[LockEvent](
    Subtype(deriveCodec[LockAdded]),
    Subtype(deriveCodec[LockUpdated]))
}
