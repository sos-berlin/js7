package com.sos.jobscheduler.master.web.master.api.fatevent

import com.sos.jobscheduler.base.time.Timestamp.now
import com.sos.jobscheduler.base.utils.CloseableIterator
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime.RichConcurrentDuration
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, EventSeq, KeyedEvent, Stamped, TearableEventSeq}
import com.sos.jobscheduler.data.fatevent.FatEvent
import com.sos.jobscheduler.master.web.master.api.fatevent.FatStateCache._
import monix.execution.Scheduler
import scala.concurrent.duration._

/** Remembers two `FatState` of (1) last requested and (2) last returned EventId.
  */
private[fatevent] class FatStateCache {
  // May be accessed by multiple clients simultaneously
  @volatile
  private var lastRequested = FatState.Initial
  @volatile
  private var lastDelivered = FatState.Initial  // Modified while on-the-fly built FatEvent stream is being sent to client !!!

  final class Accessor[FatStateCache](requestedAfter: EventId)
  {
    private var isRebuilding = false

    private var state = synchronized {
      if (requestedAfter >= lastDelivered.eventId) {
        logger.trace(s"Using last lastDelivered FatState ${EventId.toString(lastDelivered.eventId)}" +
          (if (requestedAfter > lastDelivered.eventId) s", after=${EventId.toString(requestedAfter)}" else ""))
        lastRequested = lastDelivered
        lastDelivered
      } else if (requestedAfter >= lastRequested.eventId) {
        logger.trace(s"Using last requested FatState ${EventId.toString(lastRequested.eventId)}" +
          (if (requestedAfter > lastRequested.eventId) s", after=${EventId.toString(requestedAfter)}" else ""))
        isRebuilding = true
        lastRequested
      } else {
        logger.trace("Rebuilding from initial FatState, after=${EventId.toString(after)}")
        isRebuilding = true
        FatState.Initial  // FIXME Scheitert, wenn erste Journaldatei bereits gelöscht.
      }
    }

    def eventId = state.eventId

    def toFatEventSeq(
      request: EventRequest[FatEvent],
      eventSeq: TearableEventSeq[CloseableIterator, KeyedEvent[Event]])
      (implicit scheduler: Scheduler)
    : TearableEventSeq[CloseableIterator, KeyedEvent[FatEvent]]
    =
      eventSeq match {
        case o: TearableEventSeq.Torn ⇒ o
        case o: EventSeq.Empty ⇒ o
        case EventSeq.NonEmpty(stampedIterator) ⇒
          var lastEventId = requestedAfter
          val fatCloseableIterator = stampedIterator
            .flatMap { stamped ⇒
              lastEventId = stamped.eventId
              toFatEvents(stamped)
            }
            .dropWhile { stamped ⇒
              val drop = stamped.eventId <= request.after
              watch(stamped.eventId, rebuildCompleted = !drop)  // Watch drop events used for FatState's rebuild
              drop
            }
            .take(request.limit)
          if (fatCloseableIterator.isEmpty)
            EventSeq.Empty(lastEventId)
          else
            EventSeq.NonEmpty(fatCloseableIterator)
      }

    private def toFatEvents(stamped: Stamped[KeyedEvent[Event]]): Option[Stamped[KeyedEvent[FatEvent]]] = {
      val (updated, fatEvents) = state.toFatEvents(stamped)
      state = updated
      if (state.eventId <= requestedAfter) {
        lastRequested = state
      }
      lastDelivered = state
      fatEvents
    }

    private val startedAt = now
    private var eventCount = 0
    private var stillRebuilding = false
    private var loggedTrace = false
    private var loggedLong = false

    private def watch(eventId: EventId, rebuildCompleted: Boolean): Unit = {
      eventCount += 1
      lazy val duration = now - startedAt
      if (!stillRebuilding && eventId <= requestedAfter && duration >= InfoAfter) {
        stillRebuilding = true
        logger.info(s"Still rebuilding requested FatState, $eventCount events processed since ${duration.pretty}")
      }
      if (rebuildCompleted) {
        if (isRebuilding && !loggedTrace && eventId > requestedAfter) {  // First event read?
          loggedTrace = true
          logger.trace(s"Rebuilding FatState from $eventCount events completed after ${duration.pretty}")
        }
        if (!loggedLong && duration >= 30.seconds) {
          loggedLong = true
          logger.info(s"Rebuilding FatState from $eventCount events completed after ${duration.pretty}")
        }
      }
    }
  }
}

object FatStateCache {
  private val InfoAfter = 30.seconds
  private val logger = Logger(getClass)
}
