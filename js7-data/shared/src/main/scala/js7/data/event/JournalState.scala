package js7.data.event

import io.circe.generic.semiauto.deriveCodec
import js7.base.auth.UserId
import js7.base.utils.ScalaUtils.syntax._
import js7.data.event.JournalEvent.{Heartbeat, JournalEventsReleased, SnapshotTaken}
import monix.reactive.Observable

final case class JournalState(userIdToReleasedEventId: Map[UserId, EventId])
{
  def estimatedSnapshotSize = if (this != JournalState.empty) 1 else 0

  def toSnapshotObservable =
    Observable.fromIterable((this != JournalState.empty) ? this)

  def applyEvent(event: JournalEvent): JournalState =
    event match {
      case SnapshotTaken =>
        this

      case JournalEventsReleased(userId, untilEventId) =>
        copy(userIdToReleasedEventId = userIdToReleasedEventId + (userId -> untilEventId))

      case Heartbeat => // for completeness
        this
    }

  def toReleaseEventId(acknowledegedEventId: EventId, userIds: Iterable[UserId]): EventId =
    (((userIds.map(_ -> EventId.BeforeFirst).toMap ++ userIdToReleasedEventId).values)
      ++ Array(acknowledegedEventId)
    ).min
}

object JournalState
{
  val empty = JournalState(Map.empty)
  implicit val jsonCodec = deriveCodec[JournalState]
}
