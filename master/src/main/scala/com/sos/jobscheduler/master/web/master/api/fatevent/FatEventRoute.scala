package com.sos.jobscheduler.master.web.master.api.fatevent

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.Directives.{complete, get, pathEnd, reject}
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport.jsonOrYamlMarshaller
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import com.sos.jobscheduler.common.akkahttp.StreamingSupport._
import com.sos.jobscheduler.common.event.EventReader
import com.sos.jobscheduler.common.event.collector.EventDirectives.eventRequest
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, EventSeq, KeyedEvent, TearableEventSeq}
import com.sos.jobscheduler.data.filebased.RepoEvent
import com.sos.jobscheduler.data.order.{OrderEvent, OrderFatEvent}
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

  final val fatEventRoute: Route =
    pathEnd {
      get {
        eventRequest[OrderFatEvent](defaultReturnType = Some("OrderFatEvent")).apply {
          case request: EventRequest[OrderFatEvent] ⇒
            val req = EventRequest[Event](
              Set(classOf[OrderEvent], classOf[RepoEvent]),
              after = EventId.BeforeFirst,  // TODO Provisorisch: Wir lesen jedesmal vom Anfang des Journals
              timeout = request.timeout,
              delay = request.delay,
              limit = Int.MaxValue)
            val marshallable = eventReader.read[Event](req) map {
              case o: TearableEventSeq.Torn ⇒
                ToResponseMarshallable(o: TearableEventSeq[Seq, KeyedEvent[OrderFatEvent]])

              case o: EventSeq.Empty ⇒
                ToResponseMarshallable(o: TearableEventSeq[Seq, KeyedEvent[OrderFatEvent]])

              case EventSeq.NonEmpty(stampeds) ⇒
                val converter = new StatefulEventToFatEventConverter
                val fatEvents = stampeds flatMap converter.toFatOrderEvents dropWhile (_.eventId <= request.after) take request.limit
                implicit val x = NonEmptyEventSeqJsonStreamingSupport
                ToResponseMarshallable(closeableIteratorToAkkaSource(fatEvents))
            }
            complete(marshallable)
          case _ ⇒
            reject
        }
      }
    }
}
