package com.sos.jobscheduler.core.event.journal.data

import io.circe.Json

/**
  * @author Joacim Zschimmer
  */
private[journal] object JournalHeaders
{
  val SnapshotsHeader = Json.fromString("-------SNAPSHOTS-------")
  val EventsHeader    = Json.fromString("-------EVENTS-------")
}
