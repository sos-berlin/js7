package js7.data.plan

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.time.Timestamp
import js7.data.board.NoticeEvent.NoticeDeleted
import js7.data.event.Event
import js7.data.plan.PlanEvent.*

trait PlanEvent extends Event.IsKeyBase[PlanEvent]:
  val keyCompanion: PlanEvent.type = PlanEvent


object PlanEvent extends Event.CompanionForKey[PlanId, PlanEvent]:
  given implicitSelf: PlanEvent.type = this

  sealed trait PlanStatusEvent(val status: PlanStatus) extends PlanEvent

  object PlanStatusEvent:
    inline def unapply(event: PlanStatusEvent): Some[PlanStatus] =
      Some(event.status)


  type PlanOpened = PlanOpened.type
  case object PlanOpened extends PlanStatusEvent(PlanStatus.Open)


  type PlanClosed = PlanClosed.type
  case object PlanClosed extends PlanStatusEvent(PlanStatus.Closed)


  final case class PlanFinished(at: Timestamp)
  extends PlanStatusEvent(PlanStatus.Finished(at))


  type PlanDeleted = PlanDeleted.type
  case object PlanDeleted extends PlanStatusEvent(PlanStatus.Deleted)


  given TypedJsonCodec[PlanEvent] = TypedJsonCodec[PlanEvent](
    Subtype(PlanOpened),
    Subtype(PlanClosed),
    Subtype(deriveCodec[PlanFinished]),
    Subtype(PlanDeleted))


type PlanFinishedEvent = PlanFinished | NoticeDeleted | PlanDeleted
