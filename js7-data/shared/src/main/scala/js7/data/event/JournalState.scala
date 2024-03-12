package js7.data.event

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.auth.UserId
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.event.JournalEvent.{Heartbeat, JournalEventsReleased, SnapshotTaken}
import fs2.Stream

final case class JournalState(userIdToReleasedEventId: Map[UserId, EventId]):
  def estimatedSnapshotSize = if this != JournalState.empty then 1 else 0

  def toSnapshotStream =
    Stream.iterable((this != JournalState.empty) ? this)

  def applyEvent(event: JournalEvent): JournalState =
    event match
      case SnapshotTaken =>
        this

      case JournalEventsReleased(userId, untilEventId) =>
        copy(userIdToReleasedEventId = userIdToReleasedEventId + (userId -> untilEventId))

      case Heartbeat => // for completeness
        this

  def toReleaseEventId(acknowledegedEventId: EventId, userIds: Iterable[UserId]): EventId =
    val defaults = userIds.map(_ -> EventId.BeforeFirst).toMap
    val userToEventId = defaults ++ userIdToReleasedEventId
    (userToEventId.values.view :+ acknowledegedEventId).min


object JournalState:
  val empty = JournalState(Map.empty)
  implicit val jsonCodec: Codec.AsObject[JournalState] = deriveCodec[JournalState]
