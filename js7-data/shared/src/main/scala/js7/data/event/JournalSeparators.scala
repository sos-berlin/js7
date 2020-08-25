package js7.data.event

import io.circe.Json
import js7.base.circeutils.CirceUtils.RichCirceString
import js7.base.data.ByteArray
import js7.base.data.ByteSequence.ops._
import js7.base.problem.Checked._
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.ScodecUtils.syntax._
import scodec.bits.ByteVector

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
  val EndOfJournalFileMarker: ByteVector = ByteVector.encodeUtf8("\"/// END OF JOURNAL FILE ///\"\n").orThrow
  val EndOfJournalFileMarkerJson: Json = EndOfJournalFileMarker.utf8String.parseJsonOrThrow
  val HeartbeatMarker: ByteVector = ByteVector.encodeUtf8("\"/// HEARTBEAT ///\"\n").orThrow
}
