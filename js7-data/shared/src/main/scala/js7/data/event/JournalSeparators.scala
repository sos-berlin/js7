package js7.data.event

import io.circe.Json
import js7.base.circeutils.CirceUtils.RichCirceString
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.ScodecUtils.RichByteVector
import scodec.bits.ByteVector

/**
  * @author Joacim Zschimmer
  */
object JournalSeparators
{
  val SnapshotHeader = Json.fromString("-------SNAPSHOT-------")
  val SnapshotFooter = Json.fromString("-------END OF SNAPSHOT-------")
  val EventHeader    = Json.fromString("-------EVENTS-------")
  val Transaction    = Json.fromString("TRANSACTION")
  val Commit         = Json.fromString("COMMIT")

  /** Marker to distinguish from end of  stream due to timeut.
    * The file itself must not contain this line! */
  val EndOfJournalFileMarker: ByteVector = ByteVector.encodeUtf8("\"/// END OF JOURNAL FILE ///\"\n").orThrow
  val EndOfJournalFileMarkerJson: Json = EndOfJournalFileMarker.utf8String.parseJsonOrThrow
  val HeartbeatMarker: ByteVector = ByteVector.encodeUtf8("\"/// HEARTBEAT ///\"\n").orThrow
}
