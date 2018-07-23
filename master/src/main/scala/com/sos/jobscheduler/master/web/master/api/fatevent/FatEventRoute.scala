package com.sos.jobscheduler.master.web.master.api.fatevent

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.Directives.{complete, get, pathEnd, reject}
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.base.auth.ValidUserPermission
import com.sos.jobscheduler.base.utils.CloseableIterator
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport.jsonOrYamlMarshaller
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import com.sos.jobscheduler.common.akkahttp.StreamingSupport._
import com.sos.jobscheduler.common.event.EventWatch
import com.sos.jobscheduler.common.event.collector.EventDirectives.eventRequest
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, EventSeq, KeyedEvent, Stamped, TearableEventSeq}
import com.sos.jobscheduler.data.fatevent.FatEvent
import com.sos.jobscheduler.data.filebased.RepoEvent
import com.sos.jobscheduler.data.order.OrderEvent
import com.sos.jobscheduler.master.data.events.{MasterAgentEvent, MasterEvent}
import com.sos.jobscheduler.master.web.common.MasterRouteProvider
import com.sos.jobscheduler.master.web.master.api.fatevent.FatEventRoute._
import monix.execution.Scheduler
import scala.collection.immutable.Seq

// For tests see HistoryTest

/**
  * @author Joacim Zschimmer
  */
trait FatEventRoute extends MasterRouteProvider
{
  protected def eventWatch: EventWatch[Event]

  private implicit def implicitScheduler = scheduler

  private val fatStateCache = new FatStateCache

  final val fatEventRoute: Route =
    pathEnd {
      get {
        authorizedUser(ValidUserPermission) { _ ⇒
          eventRequest[FatEvent](defaultReturnType = Some("FatEvent")).apply {
            case request: EventRequest[FatEvent] ⇒
              val stateAccessor = fatStateCache.newAccessor(request.after)
              val underlyingRequest = EventRequest[Event](
                Set(classOf[OrderEvent], classOf[RepoEvent], classOf[MasterEvent], classOf[MasterAgentEvent.AgentReady]),
                after = stateAccessor.eventId,
                timeout = request.timeout,
                delay = request.delay,
                limit = Int.MaxValue)
              val marshallable = eventWatch.read[Event](underlyingRequest) map (stateAccessor.toFatEventSeq(request, _)) map {
                case o: TearableEventSeq.Torn ⇒
                  ToResponseMarshallable(o: TearableEventSeq[Seq, KeyedEvent[FatEvent]])

                case o: EventSeq.Empty ⇒
                  ToResponseMarshallable(o: TearableEventSeq[Seq, KeyedEvent[FatEvent]])

                case EventSeq.NonEmpty(stampedIterator) ⇒
                  implicit val x = NonEmptyEventSeqJsonStreamingSupport
                  ToResponseMarshallable(closeableIteratorToAkkaSource(stampedIterator))
              }
              complete(marshallable)

            case _ ⇒
              reject
          }
        }
      }
    }
}

object FatEventRoute
{
  /** Remembers two `FatState` of (1) last requested and (2) last returned EventId.
    */
  private class FatStateCache {
    @volatile
    private var _lastRequestedState = FatState.Initial
    @volatile  // May be accessed by multiple clients simultaneously
    private var _lastState = FatState.Initial  // Modified while on-the-fly built FatEvent stream is being sent to client !!!

    def newAccessor(after: EventId) = new Accessor(after)

    final class Accessor private[FatStateCache](after: EventId) {
      private var state = _lastState  // _lastState may change
      if (after == state.eventId) {
        _lastRequestedState = state
      } else
      if (after < state.eventId) {
        state = _lastRequestedState
        if (after < state.eventId) {
          state = FatState.Initial
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
            var lastEventId = after
            val closeableIterator = stampedIterator
              .flatMap { stamped ⇒
                lastEventId = stamped.eventId
                toFatEvents(stamped)
              }
              .dropWhile(_.eventId <= request.after)
              .take(request.limit)
            if (closeableIterator.isEmpty)
              EventSeq.Empty(lastEventId)
            else
              EventSeq.NonEmpty(closeableIterator)
        }

      def toFatEvents(stamped: Stamped[KeyedEvent[Event]]): Option[Stamped[KeyedEvent[FatEvent]]] = {
        val (s, fatEvents) = state.toFatEvents(stamped)
        if (s.eventId <= after) {
          _lastRequestedState = s
        }
        state = s
        _lastState = s
        fatEvents
      }
    }
  }
}
