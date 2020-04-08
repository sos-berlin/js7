package com.sos.jobscheduler.data.event

import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}

sealed trait JournalEvent extends NoKeyEvent

object JournalEvent
{
  sealed trait SnapshotTaken extends JournalEvent
  case object SnapshotTaken extends SnapshotTaken

  final case class JournalEventsReleased(userId: UserId, untilEventId: EventId)
  extends JournalEvent

  implicit val jsonCodec = TypedJsonCodec[JournalEvent](
    Subtype(SnapshotTaken),
    Subtype(deriveCodec[JournalEventsReleased]))
}
