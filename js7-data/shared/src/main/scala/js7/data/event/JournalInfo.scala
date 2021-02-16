package js7.data.event

import js7.base.circeutils.CirceUtils.deriveCodec

final case class JournalInfo(
  lastEventId: EventId,
  tornEventId: EventId,
  journalFiles: Seq[JournalPosition]
)

object JournalInfo
{
  implicit val jsonCodec = deriveCodec[JournalInfo]
}
