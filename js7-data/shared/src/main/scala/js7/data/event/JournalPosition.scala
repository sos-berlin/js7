package js7.data.event

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

/** A byte-position in a journal consisting of multiple files.
  *
  * @param fileEventId EventId of the last event of the previous file, or EventId.BeforeFirst (0)
  * @param position Position (in bytes) in file denoted by `fileEventId`
  */
final case class JournalPosition(fileEventId: EventId, position: Long)

object JournalPosition:
  implicit val jsonCodec: Codec.AsObject[JournalPosition] = deriveCodec[JournalPosition]
