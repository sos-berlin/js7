package com.sos.jobscheduler.data.event

import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.data.event.JournalEvent.{JournalEventsReleased, SnapshotTaken}
import monix.reactive.Observable
import scala.collection.immutable.Iterable
import com.sos.jobscheduler.base.utils.ScalazStyle._

final case class JournalState(userIdToReleasedEventId: Map[UserId, EventId])
{
  def toSnapshotObservable =
    Observable.fromIterable((this != JournalState.empty) ? this)

  def applyEvent(event: JournalEvent): JournalState =
    event match {
      case SnapshotTaken =>
        this

      case JournalEventsReleased(userId, untilEventId) =>
        copy(userIdToReleasedEventId = userIdToReleasedEventId + (userId -> untilEventId))
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
