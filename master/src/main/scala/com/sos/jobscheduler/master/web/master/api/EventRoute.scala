package com.sos.jobscheduler.master.web.master.api

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversableOnce
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport.jsonOrYamlMarshaller
import com.sos.jobscheduler.common.event.EventReader
import com.sos.jobscheduler.common.event.collector.EventDirectives.eventRequest
import com.sos.jobscheduler.data.event.{Event, EventSeq, KeyedEvent, SomeEventRequest, TearableEventSeq}
import com.sos.jobscheduler.master.agent.AgentEventIdEvent
import com.sos.jobscheduler.master.configuration.KeyedEventJsonCodecs.MasterKeyedEventJsonCodec.keyedEventJsonCodec
import com.sos.jobscheduler.master.web.master.api.EventRoute._
import monix.execution.Scheduler
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
trait EventRoute
{
  protected def eventReader: EventReader[Event]
  protected implicit def scheduler: Scheduler

  final val eventRoute: Route =
    get {
      pathEnd {
        eventRequest[Event](defaultReturnType = Some("Event")).apply {
          case request: SomeEventRequest[Event] ⇒
            type ESeq = TearableEventSeq[Seq, KeyedEvent[Event]]
            intelliJuseImport(jsonOrYamlMarshaller)
            val marshallable =
              eventReader.read[Event](request, predicate = isRelevantEvent).runAsync map {
                case o: TearableEventSeq.Torn ⇒
                  ToResponseMarshallable(o: ESeq)
                case o: EventSeq.Empty ⇒
                  ToResponseMarshallable(o: ESeq)
                case EventSeq.NonEmpty(events) ⇒
                  ToResponseMarshallable(EventSeq.NonEmpty(events.toImmutableSeq): ESeq)
              }
            complete(marshallable)

          case _ ⇒
            reject
        }
      }
    }
}

object EventRoute
{
  private def isRelevantEvent(keyedEvent: KeyedEvent[_ <: Event]): Boolean =
    isRelevantEvent(keyedEvent.event)

  private def isRelevantEvent(event: Event): Boolean =
    event match {
      case (//_: OrderEvent.OrderDetachable |
            _: AgentEventIdEvent) ⇒
        false
      case _ ⇒
        true
    }
}
