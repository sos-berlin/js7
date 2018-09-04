package com.sos.jobscheduler.core.event.journal.data

import io.circe.Json

/**
  * @author Joacim Zschimmer
  */
private[journal] object JournalHeaders
{
  val SnapshotHeader = Json.fromString("-------SNAPSHOTS-------")
  val SnapshotFooter = Json.fromString("-------END OF SNAPSHOTS-------")
  val EventHeader    = Json.fromString("-------EVENTS-------")
  val EventFooter    = Json.fromString("-------END OF EVENTS-------")
  val Transaction    = Json.fromString("TRANSACTION")
  val Commit         = Json.fromString("COMMIT")
}
