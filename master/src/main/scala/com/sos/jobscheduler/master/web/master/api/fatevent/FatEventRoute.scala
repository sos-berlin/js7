package com.sos.jobscheduler.master.web.master.api.fatevent

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.Directives.{complete, get, pathEnd}
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.base.auth.ValidUserPermission
import com.sos.jobscheduler.base.time.Timestamp.now
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport.jsonOrYamlMarshaller
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import com.sos.jobscheduler.common.akkahttp.StreamingSupport._
import com.sos.jobscheduler.common.event.EventWatch
import com.sos.jobscheduler.common.event.collector.EventDirectives.eventRequest
import com.sos.jobscheduler.common.monixutils.TaskSynchronizer
import com.sos.jobscheduler.data.event.{Event, EventRequest, EventSeq, KeyedEvent, TearableEventSeq}
import com.sos.jobscheduler.data.fatevent.FatEvent
import com.sos.jobscheduler.data.filebased.RepoEvent
import com.sos.jobscheduler.data.order.OrderEvent
import com.sos.jobscheduler.master.data.events.{MasterAgentEvent, MasterEvent}
import com.sos.jobscheduler.master.web.common.MasterRouteProvider
import monix.eval.Task
import scala.collection.immutable.Seq
import scala.concurrent.blocking
import scala.concurrent.duration._

// For tests see HistoryTest

/**
  * @author Joacim Zschimmer
  */
trait FatEventRoute extends MasterRouteProvider
{
  protected def eventWatch: EventWatch[Event]

  private implicit def implicitScheduler = scheduler

  private lazy val fatStateCache = new FatStateCache(eventWatch)
  private lazy val synchronizer = new TaskSynchronizer[ToResponseMarshallable]()(scheduler)

  final val fatEventRoute: Route =
    pathEnd {
      get {
        authorizedUser(ValidUserPermission) { _ ⇒
          eventRequest[FatEvent](defaultReturnType = Some("FatEvent")).apply { fatRequest ⇒
            complete(
              synchronizer.run(  // May take a long time when rebuilding FatState
                blocking(
                  requestFatEvents(fatRequest))))
          }
        }
      }
    }

  private def requestFatEvents(fatRequest: EventRequest[FatEvent]): Task[ToResponseMarshallable] = {
    val timeoutAt = now + fatRequest.timeout
    Task {
      val stateAccessor = fatStateCache.newAccessor(fatRequest.after)

      def requestFat(underlyingRequest: EventRequest[Event]): Task[ToResponseMarshallable] =
        eventWatch.when[Event](underlyingRequest.copy[Event](timeout = timeoutAt - now)) map (stateAccessor.toFatEventSeq(fatRequest, _)) map {
          case o: TearableEventSeq.Torn ⇒
            ToResponseMarshallable(o: TearableEventSeq[Seq, KeyedEvent[FatEvent]])

          case empty: EventSeq.Empty ⇒
            val nextTimeout = timeoutAt - now
            if (nextTimeout > Duration.Zero)
              requestFat(underlyingRequest.copy[Event](after = empty.lastEventId))
            else
              ToResponseMarshallable(empty: TearableEventSeq[Seq, KeyedEvent[FatEvent]])

          case EventSeq.NonEmpty(stampedIterator) ⇒
            implicit val x = NonEmptyEventSeqJsonStreamingSupport
            closeableIteratorToMarshallable(stampedIterator)
        }

      requestFat(EventRequest[Event](
        Set(classOf[OrderEvent], classOf[RepoEvent], classOf[MasterEvent], classOf[MasterAgentEvent.AgentReady]),
        after = stateAccessor.eventId,
        timeout = fatRequest.timeout,
        delay = fatRequest.delay,
        limit = Int.MaxValue))
    }
  }
}
