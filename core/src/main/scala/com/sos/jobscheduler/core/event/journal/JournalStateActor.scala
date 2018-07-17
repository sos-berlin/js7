package com.sos.jobscheduler.core.event.journal

import akka.actor.{ActorRef, Props}
import akka.pattern.pipe
import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.core.event.journal.JournalStateActor._
import com.sos.jobscheduler.data.event.JournalStateEvent.EventsAccepted
import com.sos.jobscheduler.data.event.{EventId, JournalStateEvent}
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
private class JournalStateActor(protected val journalActor: ActorRef, initialState: JournalState, eventReaderOption: Option[JournalEventReader[_]])
extends KeyedEventJournalingActor[JournalStateEvent]
{
  import context.dispatcher

  private var journalState = initialState

  protected def snapshots = Future.successful(journalState :: Nil)

  def receive = journaling orElse {
    case Input.EventsAccepted(untilEventId) ⇒
      onEventsAccepted(untilEventId) pipeTo sender()

    case Input.Get ⇒
      sender() ! journalState
  }

  private def onEventsAccepted(untilEventId: EventId): Future[Checked[Completed]] =
    if (untilEventId < journalState.eventsAcceptedUntil)
      Future.successful(Invalid(Problem(
        s"EventsAccepted($untilEventId) out of order, an EventId not below ${journalState.eventsAcceptedUntil} is expected")))
    else
      persist(EventsAccepted(untilEventId)) { stamped ⇒
        journalState = journalState.handleEvent(stamped.value.event)
        for (r ← eventReaderOption) r.onEventsAcceptedUntil(untilEventId)
        Valid(Completed)
      }
}

private[journal] object JournalStateActor
{
  def props(journalActor: ActorRef, initialState: JournalState, eventReaderOption: Option[JournalEventReader[_]]) =
    Props { new JournalStateActor(journalActor, initialState, eventReaderOption: Option[JournalEventReader[_]]) }

  object Input {
    final case class EventsAccepted(untilEventId: EventId)
    final case object Get
  }
}
