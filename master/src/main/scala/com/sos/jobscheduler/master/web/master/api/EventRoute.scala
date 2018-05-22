package com.sos.jobscheduler.master.web.master.api

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.marshalling.sse.EventStreamMarshalling._
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.MediaTypes.`text/event-stream`
import akka.http.scaladsl.model.StatusCodes.BadRequest
import akka.http.scaladsl.model.headers.`Last-Event-ID`
import akka.http.scaladsl.model.sse.ServerSentEvent
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive1, Route}
import com.sos.jobscheduler.base.circeutils.CirceUtils.CompactPrinter
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.accept
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport.jsonOrYamlMarshaller
import com.sos.jobscheduler.common.akkahttp.HttpStatusCodeException
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import com.sos.jobscheduler.common.akkahttp.StreamingSupport._
import com.sos.jobscheduler.common.akkahttp.html.HtmlDirectives.htmlPreferred
import com.sos.jobscheduler.common.event.EventReader
import com.sos.jobscheduler.common.event.collector.EventDirectives
import com.sos.jobscheduler.common.event.collector.EventDirectives.eventRequest
import com.sos.jobscheduler.common.http.CirceJsonSeqSupport.{`application/json-seq`, jsonSeqMarshaller}
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, EventSeq, KeyedEvent, SomeEventRequest, Stamped, TearableEventSeq}
import com.sos.jobscheduler.master.agent.AgentEventIdEvent
import com.sos.jobscheduler.master.configuration.KeyedEventJsonCodecs.MasterKeyedEventJsonCodec
import com.sos.jobscheduler.master.web.master.api.EventRoute._
import io.circe.syntax.EncoderOps
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
        htmlPreferred {
          oneShot
        } ~
        accept(`application/json-seq`) {
          jsonSeqEvents
        } ~
        accept(`text/event-stream`) {
          serverSentEvents
        } ~
        oneShot
      }
    }

  private def oneShot: Route =
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

  private def jsonSeqEvents: Route =
    eventDirective(defaultTimeout = DefaultJsonSeqChunkTimeout, defaultDelay = Duration.Zero) {
      case request: EventRequest[Event] ⇒
        implicit val x = JsonSeqStreamSupport
        implicit val y = jsonSeqMarshaller[Stamped[KeyedEvent[Event]]]
        complete(eventReader.observe(request, predicate = isRelevantEvent))

      case _ ⇒
        reject
    }

  private def serverSentEvents: Route =
    parameter("v" ? BuildInfo.buildId) { requestedBuildId ⇒
      if (requestedBuildId != BuildInfo.buildId)
        complete(HttpEntity(
          `text/event-stream`,
          s"data:${Problem("BUILD-CHANGED").asJson.pretty(CompactPrinter)}\n\n"))  // Exact this message is checked in experimental GUI
      else
        eventDirective(defaultTimeout = DefaultJsonSeqChunkTimeout) {
          case request: EventRequest[Event] ⇒
            optionalHeaderValueByType[`Last-Event-ID`](()) { lastEventIdHeader ⇒
              val req = lastEventIdHeader.fold(request)(header ⇒
                request.copy[Event](after = toLastEventId(header)))
              val mutableJsonPrinter = CompactPrinter.copy(reuseWriters = true)
              val source = eventReader.observe(req, predicate = isRelevantEvent)
                .map(stamped ⇒ ServerSentEvent(
                  data = stamped.asJson.pretty(mutableJsonPrinter),
                  id = Some(stamped.eventId.toString)))
                .toAkkaSource
              complete(source)
            }

          case _ ⇒
            reject
        }
    }
}

object EventRoute
{
  private val DefaultJsonSeqChunkTimeout = 24.hours  // Renewed for each chunked

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

  private def toLastEventId(header: `Last-Event-ID`): EventId =
    try java.lang.Long.parseLong(header.id)
    catch {
      case e: NumberFormatException ⇒
        throw new HttpStatusCodeException(BadRequest, s"Invalid header Last-Event-Id: $e")
    }
}
