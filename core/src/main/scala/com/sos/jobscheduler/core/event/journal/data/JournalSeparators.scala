package com.sos.jobscheduler.core.event.journal.data

import io.circe.Json

/**
  * @author Joacim Zschimmer
  */
object JournalSeparators
{
  val SnapshotHeader = Json.fromString("-------SNAPSHOT-------")
  val SnapshotFooter = Json.fromString("-------END OF SNAPSHOT-------")
  val EventHeader    = Json.fromString("-------EVENTS-------")
  val EventFooter    = Json.fromString("-------END OF EVENTS-------")
  val Transaction    = Json.fromString("TRANSACTION")
  val Commit         = Json.fromString("COMMIT")
}
