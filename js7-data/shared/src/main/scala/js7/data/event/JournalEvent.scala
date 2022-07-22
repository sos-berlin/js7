package js7.data.event

import io.circe.generic.semiauto.deriveCodec
import js7.base.auth.UserId
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}

sealed trait JournalEvent extends NoKeyEvent

object JournalEvent
{
  type SnapshotTaken = SnapshotTaken.type
  case object SnapshotTaken extends JournalEvent

  final case class JournalEventsReleased(userId: UserId, untilEventId: EventId)
  extends JournalEvent

  case object Heartbeat
  extends JournalEvent

  final val StampedHeartbeat: Stamped[KeyedEvent[JournalEvent]] =
    Stamped(0, Heartbeat)

  implicit val jsonCodec: TypedJsonCodec[JournalEvent] = TypedJsonCodec(
    Subtype(SnapshotTaken),
    Subtype(deriveCodec[JournalEventsReleased]),
    Subtype(Heartbeat))
}
