package js7.data.event

import io.circe.Json
import js7.base.data.ByteArray
import js7.base.problem.Checked.*

/**
  * @author Joacim Zschimmer
  */
object JournalSeparators:
  val SnapshotHeaderLine = ByteArray("\"-------SNAPSHOT-------\"\n")
  val SnapshotFooterLine = ByteArray("\"-------END OF SNAPSHOT-------\"\n")
  val EventHeaderLine    = ByteArray("\"-------EVENTS-------\"\n")

  val SnapshotHeader = SnapshotHeaderLine.parseJson.orThrow
  val SnapshotFooter = SnapshotFooterLine.parseJson.orThrow
  val EventHeader    = EventHeaderLine.parseJson.orThrow
  val Transaction    = Json.fromString("TRANSACTION")
  val Commit         = Json.fromString("COMMIT")

  /** Marker to distinguish from end of stream due to timeout.
    * The file itself must not contain this line! */
  val EndOfJournalFileMarker = ByteArray("\"/// END OF JOURNAL FILE ///\"\n")
