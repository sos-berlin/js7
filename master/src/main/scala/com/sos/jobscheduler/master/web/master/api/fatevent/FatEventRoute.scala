package com.sos.jobscheduler.master.web.master.api.fatevent

import akka.http.scaladsl.server.Directives.{complete, get, pathEnd, reject}
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversableOnce
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport.jsonOrYamlMarshaller
import com.sos.jobscheduler.common.event.EventReader
import com.sos.jobscheduler.common.event.collector.EventDirectives.eventRequest
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, EventSeq, KeyedEvent, TearableEventSeq}
import com.sos.jobscheduler.data.filebased.RepoEvent
import com.sos.jobscheduler.data.order.{OrderEvent, OrderFatEvent}
import com.sos.jobscheduler.master.order.fat.StatefulEventToFatOrderEventConverter
import monix.execution.Scheduler
import scala.collection.immutable.Seq
import scala.concurrent.Future

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
            complete(readFatEvents(eventReader, request))
          case _ ⇒
            reject
        }
      }
    }

  private def readFatEvents(eventReader: EventReader[Event], request: EventRequest[OrderFatEvent]): Future[TearableEventSeq[Seq, KeyedEvent[OrderFatEvent]]] = {
    val req = EventRequest[Event](
      Set(classOf[OrderEvent], classOf[RepoEvent]),
      after = EventId.BeforeFirst,  // TODO Provisorisch: Wir lesen jedesmal vom Anfang des Journals
      timeout = request.timeout, request.delay,
      limit = Int.MaxValue)
    val converter = new StatefulEventToFatOrderEventConverter
    eventReader.byPredicate[Event](req, predicate = _ ⇒ true)
      .map {
        case o: TearableEventSeq.Torn ⇒ o
        case o: EventSeq.Empty ⇒ o
        case EventSeq.NonEmpty(stampeds) ⇒
          val fat = stampeds flatMap converter.toFatOrderEvents dropWhile (_.eventId <= request.after) take request.limit
          EventSeq.NonEmpty(fat.toImmutableSeq)
      }
  }
}
