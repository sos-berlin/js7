package js7.data.event

import io.circe.Json
import io.circe.generic.semiauto.deriveCodec
import js7.base.auth.UserId
import js7.base.circeutils.CirceUtils.RichCirceString
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.data.ByteArray
import js7.base.fs2utils.StreamExtensions.stringAsUtf8
import js7.base.utils.ScalaUtils.syntax.RichEither

sealed trait JournalEvent extends NoKeyEvent


object JournalEvent :
  type SnapshotTaken = SnapshotTaken.type
  case object SnapshotTaken extends JournalEvent

  final case class JournalEventsReleased(userId: UserId, untilEventId: EventId)
  extends JournalEvent

  // Heartbeat occurs only in communication but not in a journal file.

  case object Heartbeat
  extends JournalEvent

  val StampedHeartbeat: Stamped[KeyedEvent[JournalEvent]] = Stamped(0, Heartbeat)
  val StampedHeartbeatString: String = """{"eventId":0,"TYPE":"Heartbeat"}""" + "\n"
  val StampedHeartbeatByteArray: ByteArray = ByteArray(StampedHeartbeatString)
  val StampedHeartbeatFs2Chunk: fs2.Chunk[Byte] =
    fs2.Chunk.stringAsUtf8(StampedHeartbeatString)
  val StampedHeartbeatJson: Json = StampedHeartbeatString.parseJsonAs[Json].orThrow

  implicit val jsonCodec: TypedJsonCodec[JournalEvent] = TypedJsonCodec(
    Subtype(SnapshotTaken),
    Subtype(deriveCodec[JournalEventsReleased]),
    Subtype(Heartbeat))
