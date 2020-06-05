package js7.data.event

import js7.base.auth.UserId
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}

sealed trait JournalEvent extends NoKeyEvent

object JournalEvent
{
  type SnapshotTaken = SnapshotTaken.type
  case object SnapshotTaken extends JournalEvent

  final case class JournalEventsReleased(userId: UserId, untilEventId: EventId)
  extends JournalEvent

  implicit val jsonCodec = TypedJsonCodec[JournalEvent](
    Subtype(SnapshotTaken),
    Subtype(deriveCodec[JournalEventsReleased]))
}
