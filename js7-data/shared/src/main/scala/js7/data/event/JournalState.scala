package js7.data.event

import fs2.{Pure, Stream}
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.auth.UserId
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.event
import js7.data.event.JournalEvent.{Heartbeat, JournalEventsReleased, SnapshotTaken}

final case class JournalState(userIdToReleasedEventId: Map[UserId, EventId])
extends EventDriven[JournalState, JournalEvent]:

  def companion: JournalState.type = JournalState

  def estimatedSnapshotSize: Int =
    if this != JournalState.empty then 1 else 0

  def toSnapshotStream: Stream[Pure, JournalState] =
    Stream.iterable((this != JournalState.empty) ? this)

  def applyEvent(event: JournalEvent): Right[Nothing, JournalState] =
    Right:
      event match
        case SnapshotTaken =>
          this

        case JournalEventsReleased(userId, untilEventId) =>
          copy(userIdToReleasedEventId = userIdToReleasedEventId + (userId -> untilEventId))

        case Heartbeat => // for completeness
          this

  def toReleaseEventId(acknowledgedEventId: EventId, userIds: Iterable[UserId]): EventId =
    val defaults = userIds.map(_ -> EventId.BeforeFirst).toMap
    val userToEventId = defaults ++ userIdToReleasedEventId
    (userToEventId.values.view :+ acknowledgedEventId).min


object JournalState extends EventDriven.Companion[JournalState, JournalEvent]:

  val empty: JournalState = JournalState(Map.empty)
  implicit val jsonCodec: Codec.AsObject[JournalState] = deriveCodec[JournalState]
