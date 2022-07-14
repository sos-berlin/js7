package js7.data.event

import io.circe.Json
import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils.{RichCirceString, RichJson}
import js7.base.data.ByteArray
import js7.base.problem.Checked.*
import js7.data.event.JournalEvent.StampedHeartbeat

/**
  * @author Joacim Zschimmer
  */
object JournalSeparators
{
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
  val EndOfJournalFileMarkerJson: Json = EndOfJournalFileMarker.utf8String.parseJsonOrThrow
  val HeartbeatMarker = ByteArray("""{"eventId":0,"TYPE":"Heartbeat"}""" + "\n")

  // Serialization must equal HeartbeatMarker
  assert(HeartbeatMarker.parseJsonAs[Stamped[KeyedEvent[JournalEvent]]] == Right(StampedHeartbeat))
  assert(StampedHeartbeat.asJson.toByteArray ++ ByteArray("\n") == HeartbeatMarker)
}
