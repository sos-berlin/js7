package com.sos.jobscheduler.master.web.master.api.fatevent

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.Directives.{complete, get, pathEnd}
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.base.auth.ValidUserPermission
import com.sos.jobscheduler.base.time.Timestamp.now
import com.sos.jobscheduler.base.utils.CloseableIterator
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport.jsonOrYamlMarshaller
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import com.sos.jobscheduler.common.akkahttp.StreamingSupport._
import com.sos.jobscheduler.common.event.EventWatch
import com.sos.jobscheduler.common.event.collector.EventDirectives.eventRequest
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime.RichConcurrentDuration
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, EventSeq, KeyedEvent, Stamped, TearableEventSeq}
import com.sos.jobscheduler.data.fatevent.FatEvent
import com.sos.jobscheduler.data.filebased.RepoEvent
import com.sos.jobscheduler.data.order.OrderEvent
import com.sos.jobscheduler.master.data.events.{MasterAgentEvent, MasterEvent}
import com.sos.jobscheduler.master.web.common.MasterRouteProvider
import com.sos.jobscheduler.master.web.master.api.fatevent.FatEventRoute._
import monix.eval.Task
import monix.execution.Scheduler
import scala.collection.immutable.Seq
import scala.concurrent.duration._

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
          eventRequest[FatEvent](defaultReturnType = Some("FatEvent")).apply { fatRequest ⇒
            val timeoutAt = now + fatRequest.timeout
            val stateAccessor = new fatStateCache.Accessor(fatRequest.after)

            def requestFat(underlyingRequest: EventRequest[Event]): Task[ToResponseMarshallable] =
              eventWatch.when[Event](underlyingRequest) map (stateAccessor.toFatEventSeq(fatRequest, _)) map {
                case o: TearableEventSeq.Torn ⇒
                  ToResponseMarshallable(o: TearableEventSeq[Seq, KeyedEvent[FatEvent]])

                case empty: EventSeq.Empty ⇒
                  val nextTimeout = timeoutAt - now
                  if (nextTimeout > Duration.Zero)
                    requestFat(underlyingRequest.copy[Event](after = empty.lastEventId, timeout = nextTimeout))
                  else
                    ToResponseMarshallable(empty: TearableEventSeq[Seq, KeyedEvent[FatEvent]])

                case EventSeq.NonEmpty(stampedIterator) ⇒
                  implicit val x = NonEmptyEventSeqJsonStreamingSupport
                  ToResponseMarshallable(closeableIteratorToAkkaSource(stampedIterator
                    .map { stamped ⇒
                      stateAccessor.watch(stamped.eventId)
                      stamped
                    }))
              }

            complete(requestFat(EventRequest[Event](
              Set(classOf[OrderEvent], classOf[RepoEvent], classOf[MasterEvent], classOf[MasterAgentEvent.AgentReady]),
              after = stateAccessor.eventId,
              timeout = fatRequest.timeout,
              delay = fatRequest.delay,
              limit = Int.MaxValue)))
          }
        }
      }
    }
}

object FatEventRoute
{
  private val logger = Logger(getClass)

  /** Remembers two `FatState` of (1) last requested and (2) last returned EventId.
    */
  private class FatStateCache {
    @volatile
    private var lastRequestedState = FatState.Initial
    @volatile  // May be accessed by multiple clients simultaneously
    private var lastState = FatState.Initial  // Modified while on-the-fly built FatEvent stream is being sent to client !!!

    final class Accessor[FatStateCache](after: EventId)
    {
      private var isRebuilding = false
      private var state = {
        val last = lastState  // lastState may change
        if (after >= last.eventId) {
          logger.trace(s"Continuing with lastState=${EventId.toString(last.eventId)} after=${EventId.toString(after)} ")
          lastRequestedState = last
          last
        } else if (after < lastRequestedState.eventId) {
          logger.trace("Continuing with FatState.Initial")
          isRebuilding = true
          FatState.Initial  // FIXME Scheitert, wenn erste Journaldatei bereits gelöscht.
        } else {
          logger.trace(s"Continuing with lastRequestedState=${EventId.toString(last.eventId)} after=${EventId.toString(after)} ")
          isRebuilding = true
          lastRequestedState
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
            val fatCloseableIterator = stampedIterator
              .flatMap { stamped ⇒
                lastEventId = stamped.eventId
                toFatEvents(stamped)
              }
              .dropWhile(_.eventId <= request.after)
              .take(request.limit)
            if (fatCloseableIterator.isEmpty)
              EventSeq.Empty(lastEventId)
            else
              EventSeq.NonEmpty(fatCloseableIterator)
        }

      def toFatEvents(stamped: Stamped[KeyedEvent[Event]]): Option[Stamped[KeyedEvent[FatEvent]]] = {
        val (s, fatEvents) = state.toFatEvents(stamped)
        if (s.eventId <= after) {
          lastRequestedState = s
        }
        state = s
        lastState = s
        fatEvents
      }

      private val startedAt = now
      private var loggedRebuilding = false
      private var loggedTrace = false
      private var loggedLong = false

      def watch(eventId: EventId): Unit =
        if (isRebuilding) {
          lazy val duration = now - startedAt
          if (!loggedRebuilding && eventId <= after && duration >= 10.seconds) {
            loggedRebuilding = true
            logger.info(s"Still rebuilding requested FatState, since ${duration.pretty}")
          }
          if (!loggedTrace && eventId > after) {
            loggedTrace = true
            logger.trace(s"Rebuilding FatState completed after ${duration.pretty}")
            if (!loggedLong && duration >= 30.seconds) {
              loggedLong = true
              logger.info(s"Rebuilding FatState completed after ${duration.pretty}")
            }
          }
        }
    }
  }
}
