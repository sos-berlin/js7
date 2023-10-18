package js7.data.event

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

final case class JournalInfo(
  lastEventId: EventId,
  tornEventId: EventId,
  journalFiles: Seq[JournalPosition])


object JournalInfo:
  implicit val jsonCodec: Codec.AsObject[JournalInfo] = deriveCodec[JournalInfo]
