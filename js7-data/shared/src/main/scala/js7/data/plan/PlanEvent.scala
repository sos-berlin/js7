package js7.data.plan

import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.data.board.NoticeEvent.NoticeDeleted
import js7.data.event.Event
import js7.data.plan.PlanEvent.*

trait PlanEvent extends Event.IsKeyBase[PlanEvent]:
  val keyCompanion: PlanEvent.type = PlanEvent


object PlanEvent extends Event.CompanionForKey[PlanId, PlanEvent]:
  given implicitSelf: PlanEvent.type = this

  sealed trait PlanStatusEvent extends PlanEvent:
    def status: Plan.Status

  object PlanStatusEvent:
    inline def unapply(event: PlanStatusEvent): Some[Plan.Status] =
      Some(event.status)


  type PlanReopened = PlanOpened.type
  case object PlanOpened extends PlanStatusEvent:
    def status = Plan.Status.Open


  type PlanClosed = PlanClosed.type
  case object PlanClosed extends PlanStatusEvent:
    def status = Plan.Status.Closed


  type PlanFinished = PlanFinished.type
  case object PlanFinished extends PlanStatusEvent:
    def status = Plan.Status.Finished


  type PlanDeleted = PlanDeleted.type
  case object PlanDeleted extends PlanStatusEvent:
    def status = Plan.Status.Deleted


  given TypedJsonCodec[PlanEvent] = TypedJsonCodec[PlanEvent](
    Subtype(PlanOpened),
    Subtype(PlanClosed),
    Subtype(PlanFinished),
    Subtype(PlanDeleted))


type PlanFinishedEvent = PlanFinished | NoticeDeleted | PlanDeleted
