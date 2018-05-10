package com.sos.jobscheduler.master.web.master.api.fatevent

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.Directives.{complete, get, pathEnd, reject}
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport.jsonOrYamlMarshaller
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import com.sos.jobscheduler.common.akkahttp.StreamingSupport._
import com.sos.jobscheduler.common.event.EventReader
import com.sos.jobscheduler.common.event.collector.EventDirectives.eventRequest
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, EventSeq, KeyedEvent, Stamped, TearableEventSeq}
import com.sos.jobscheduler.data.filebased.RepoEvent
import com.sos.jobscheduler.data.order.{OrderEvent, OrderFatEvent}
import com.sos.jobscheduler.master.web.master.api.fatevent.FatEventRoute._
import monix.execution.Scheduler
import scala.collection.immutable.Seq

// For tests see HistoryTest

/**
  * @author Joacim Zschimmer
  */
trait FatEventRoute
{
  protected def eventReader: EventReader[Event]
  protected implicit def scheduler: Scheduler

  private val globalState = new GlobalState

  final val fatEventRoute: Route =
    pathEnd {
      get {
        eventRequest[OrderFatEvent](defaultReturnType = Some("OrderFatEvent")).apply {
          case request: EventRequest[OrderFatEvent] ⇒
            val stateAccessor = globalState.newAccessor(request.after)
            val req = EventRequest[Event](
              Set(classOf[OrderEvent], classOf[RepoEvent]),
              after = stateAccessor.eventId,
              timeout = request.timeout,
              delay = request.delay,
              limit = Int.MaxValue)
            val marshallable = eventReader.read[Event](req) map {
              case o: TearableEventSeq.Torn ⇒
                ToResponseMarshallable(o: TearableEventSeq[Seq, KeyedEvent[OrderFatEvent]])

              case o: EventSeq.Empty ⇒
                ToResponseMarshallable(o: TearableEventSeq[Seq, KeyedEvent[OrderFatEvent]])

              case EventSeq.NonEmpty(stampedIterator) ⇒
                val fatIterator = stampedIterator
                  .flatMap(stateAccessor.toFatOrderEvents)
                  .dropWhile(_.eventId <= request.after)
                  .take(request.limit)
                implicit val x = NonEmptyEventSeqJsonStreamingSupport
                ToResponseMarshallable(closeableIteratorToAkkaSource(fatIterator))
            }
            complete(marshallable)

          case _ ⇒
            reject
        }
      }
    }
}

object FatEventRoute
{
  private class GlobalState {
    @volatile
    private var _lastRequestedState = FatState.Initial
    @volatile  // May be accessed by multiple clients simultaneously
    private var _lastState = FatState.Initial  // Modified while on-the-fly built FatEvent stream is being sent to client !!!

    def newAccessor(after: EventId) = new GlobaleStateAccessor(after)

    final class GlobaleStateAccessor private[GlobalState](after: EventId) {
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

      def toFatOrderEvents(stamped: Stamped[KeyedEvent[Event]]): Seq[Stamped[KeyedEvent[OrderFatEvent]]] = {
        val (s, fatEvents) = state.toFatOrderEvents(stamped)
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
