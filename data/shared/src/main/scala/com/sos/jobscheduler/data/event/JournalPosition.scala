package com.sos.jobscheduler.data.event

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec

/** A byte-position in a journal consisting of multiple files.
  *
  * @param fileEventId EventId of the last event of the previous file, or EventId.BeforeFirst (0)
  * @param position Position (in bytes) in file denoted by `fileEventId`
  */
final case class JournalPosition(fileEventId: EventId, position: Long)

object JournalPosition {
  implicit val jsonCodec = deriveCodec[JournalPosition]
}
