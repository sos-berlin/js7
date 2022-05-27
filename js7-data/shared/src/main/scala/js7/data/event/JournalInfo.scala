package js7.data.event

import io.circe.generic.semiauto.deriveCodec

final case class JournalInfo(
  lastEventId: EventId,
  tornEventId: EventId,
  journalFiles: Seq[JournalPosition]
)

object JournalInfo
{
  implicit val jsonCodec = deriveCodec[JournalInfo]
}
