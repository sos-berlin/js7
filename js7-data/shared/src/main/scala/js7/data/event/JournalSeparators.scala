package js7.data.event

import io.circe.Json
import js7.base.data.ByteArray
import js7.base.problem.Checked.*

/**
  * @author Joacim Zschimmer
  */
object JournalSeparators:
  val SnapshotHeaderLine: ByteArray = ByteArray("\"-------SNAPSHOT-------\"\n")
  val SnapshotFooterLine: ByteArray = ByteArray("\"-------END OF SNAPSHOT-------\"\n")
  val EventHeaderLine: ByteArray = ByteArray("\"-------EVENTS-------\"\n")

  val SnapshotHeader: Json = SnapshotHeaderLine.parseJson.orThrow
  val SnapshotFooter: Json = SnapshotFooterLine.parseJson.orThrow
  val EventHeader: Json = EventHeaderLine.parseJson.orThrow
  val Transaction: Json = Json.fromString("TRANSACTION")
  val Commit: Json = Json.fromString("COMMIT")

  /** Marker to distinguish from end of stream due to timeout.
    * The file itself must not contain this line! */
  val EndOfJournalFileMarker: ByteArray = ByteArray("\"/// END OF JOURNAL FILE ///\"\n")
