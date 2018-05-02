package com.sos.jobscheduler.master.web.master.api

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive1, Route}
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.accept
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport.jsonOrYamlMarshaller
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import com.sos.jobscheduler.common.akkahttp.StreamingSupport._
import com.sos.jobscheduler.common.event.EventReader
import com.sos.jobscheduler.common.event.collector.EventDirectives
import com.sos.jobscheduler.common.event.collector.EventDirectives.eventRequest
import com.sos.jobscheduler.common.http.CirceJsonSeqSupport.{`application/json-seq`, jsonSeqMarshaller}
import com.sos.jobscheduler.data.event.{Event, EventRequest, EventSeq, KeyedEvent, SomeEventRequest, Stamped, TearableEventSeq}
import com.sos.jobscheduler.master.agent.AgentEventIdEvent
import com.sos.jobscheduler.master.configuration.KeyedEventJsonCodecs.MasterKeyedEventJsonCodec
import com.sos.jobscheduler.master.web.master.api.EventRoute._
import monix.execution.Scheduler
import scala.collection.immutable.Seq
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
trait EventRoute
{
  protected def eventReader: EventReader[Event]
  protected implicit def scheduler: Scheduler

  private def eventDirective(
    defaultTimeout: FiniteDuration = EventDirectives.DefaultTimeout,
    defaultDelay: FiniteDuration = EventDirectives.DefaultDelay)
  =
    new Directive1[SomeEventRequest[Event]] {
      def tapply(inner: Tuple1[SomeEventRequest[Event]] ⇒ Route) =
        onSuccess(eventReader.whenRealEventReader) { realEventReader ⇒
          eventRequest[Event](
            defaultAfter = Some(realEventReader.lastAddedEventId /* == 0 until JournalActor is up and running !!!*/),
            defaultDelay = defaultDelay,
            defaultTimeout = defaultTimeout,
            defaultReturnType = Some("Event"))
          .apply(eventRequest ⇒ inner(Tuple1(eventRequest)))
        }
    }

  final val eventRoute: Route =
    get {
      pathEnd {
        accept(`application/json-seq`) {
          eventDirective(defaultTimeout = DefaultJsonSeqChunkTimeout, defaultDelay = DefaultJsonSeqChunkDelay) {
            case request: EventRequest[Event] ⇒
              implicit val x = JsonSeqStreamSupport
              implicit val y = jsonSeqMarshaller[Stamped[KeyedEvent[Event]]]
              complete(eventReader.observe(request, predicate = isRelevantEvent))

            case _ ⇒
              reject
          }
        } ~
        eventDirective() { request ⇒
          intelliJuseImport(jsonOrYamlMarshaller)
          val marshallable = eventReader.read[Event](request, predicate = isRelevantEvent) map {
            case o: TearableEventSeq.Torn ⇒
              ToResponseMarshallable(o: TearableEventSeq[Seq, KeyedEvent[Event]])

            case o: EventSeq.Empty ⇒
              ToResponseMarshallable(o: TearableEventSeq[Seq, KeyedEvent[Event]])

            case EventSeq.NonEmpty(events) ⇒
              implicit val x = NonEmptyEventSeqJsonStreamingSupport
              ToResponseMarshallable(closeableIteratorToAkkaSource(events))
          }
          complete(marshallable)
        }
      }
    }
}

object EventRoute
{
  private val DefaultJsonSeqChunkTimeout = 24.hours  // Renewed for each chunked
  private val DefaultJsonSeqChunkDelay = 0.seconds

  private def isRelevantEvent(keyedEvent: KeyedEvent[_ <: Event]): Boolean =
    isRelevantEvent(keyedEvent.event)

  private def isRelevantEvent(event: Event): Boolean =
    event match {
      case //_: OrderEvent.OrderDetachable |
        _: AgentEventIdEvent ⇒
        false
      case _ ⇒
        true
    }
}
