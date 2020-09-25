package js7.data.event

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.data.event.JournalInfo._

final case class JournalInfo(
  lastEventId: EventId,
  tornEventId: EventId,
  journalFiles: Seq[JournalFileInfo]
)

object JournalInfo
{
  final case class JournalFileInfo(
    eventId: EventId,
    size: Long)
  object JournalFileInfo {
    implicit val jsonCodec = deriveCodec[JournalFileInfo]
  }

  implicit val jsonCodec = deriveCodec[JournalInfo]
}
