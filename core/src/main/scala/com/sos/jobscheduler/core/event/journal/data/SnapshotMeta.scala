package com.sos.jobscheduler.core.event.journal.data

import com.sos.jobscheduler.base.circeutils.CirceUtils
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.data.event.EventId

/**
  * @author Joacim Zschimmer
  */
final case class SnapshotMeta(eventId: EventId)

object SnapshotMeta {
  implicit val jsonCodec = TypedJsonCodec[SnapshotMeta](
    Subtype(CirceUtils.deriveCodec[SnapshotMeta])
  )
}
