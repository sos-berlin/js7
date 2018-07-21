package com.sos.jobscheduler.core.event

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.marshalling.sse.EventStreamMarshalling._
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.MediaTypes.`text/event-stream`
import akka.http.scaladsl.model.StatusCodes.BadRequest
import akka.http.scaladsl.model.headers.`Last-Event-ID`
import akka.http.scaladsl.model.sse.ServerSentEvent
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive1, Route}
import com.sos.jobscheduler.base.auth.{UserId, ValidUserPermission}
import com.sos.jobscheduler.base.circeutils.CirceUtils.CompactPrinter
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.accept
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport.jsonOrYamlMarshaller
import com.sos.jobscheduler.common.akkahttp.HttpStatusCodeException
import com.sos.jobscheduler.common.akkahttp.StandardDirectives.routeFuture
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import com.sos.jobscheduler.common.akkahttp.StreamingSupport._
import com.sos.jobscheduler.common.akkahttp.html.HtmlDirectives.htmlPreferred
import com.sos.jobscheduler.common.akkahttp.web.session.RouteProvider
import com.sos.jobscheduler.common.event.EventWatch
import com.sos.jobscheduler.common.event.collector.EventDirectives
import com.sos.jobscheduler.common.event.collector.EventDirectives.eventRequest
import com.sos.jobscheduler.common.http.CirceJsonSeqSupport.{`application/json-seq`, jsonSeqMarshaller}
import com.sos.jobscheduler.core.event.AbstractEventRoute._
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, EventSeq, KeyedEvent, KeyedEventTypedJsonCodec, SomeEventRequest, Stamped, TearableEventSeq}
import io.circe.syntax.EncoderOps
import monix.eval.Task
import scala.collection.immutable.Seq
import scala.concurrent.duration._
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
trait AbstractEventRoute[E <: Event] extends RouteProvider
{
  protected def eventClass: Class[E]
  protected implicit def keyedEventTypedJsonCodec: KeyedEventTypedJsonCodec[E]
  protected def eventWatchFor(userId: UserId): Task[EventWatch[E]]
  protected def isRelevantEvent(keyedEvent: KeyedEvent[E]): Boolean = true

  private implicit def implicitScheduler = scheduler

  private implicit def eventClassTag = ClassTag[E](eventClass)

  final val abstractEventRoute: Route =
    get {
      pathEnd {
        authorizedUser(ValidUserPermission) { user ⇒
          routeFuture(
            eventWatchFor(user.id).runAsync.map { eventWatch ⇒
              htmlPreferred {
                oneShot(eventWatch)
              } ~
              accept(`application/json-seq`) {
                jsonSeqEvents(eventWatch)
              } ~
              accept(`text/event-stream`) {
                serverSentEvents(eventWatch)
              } ~
              oneShot(eventWatch)
            })
        }
      }
    }

  private def oneShot(eventWatch: EventWatch[E]): Route =
    eventDirective(eventWatch.lastAddedEventId) { request ⇒
      intelliJuseImport(jsonOrYamlMarshaller)
      val marshallable = eventWatch.read[E](request, predicate = isRelevantEvent) map {
        case o: TearableEventSeq.Torn ⇒
          ToResponseMarshallable(o: TearableEventSeq[Seq, KeyedEvent[E]])

        case o: EventSeq.Empty ⇒
          ToResponseMarshallable(o: TearableEventSeq[Seq, KeyedEvent[E]])

        case EventSeq.NonEmpty(events) ⇒
          implicit val x = NonEmptyEventSeqJsonStreamingSupport
          ToResponseMarshallable(closeableIteratorToAkkaSource(events))
      }
      complete(marshallable)
    }

  private def jsonSeqEvents(eventWatch: EventWatch[E]): Route =
    eventDirective(eventWatch.lastAddedEventId, defaultTimeout = DefaultJsonSeqChunkTimeout, defaultDelay = Duration.Zero) {
      case request: EventRequest[E] ⇒
        implicit val x = JsonSeqStreamSupport
        implicit val y = jsonSeqMarshaller[Stamped[KeyedEvent[E]]]
        complete(eventWatch.observe(request, predicate = isRelevantEvent))

      case _ ⇒
        reject
    }

  private def serverSentEvents(eventWatch: EventWatch[E]): Route =
    parameter("v" ? BuildInfo.buildId) { requestedBuildId ⇒
      if (requestedBuildId != BuildInfo.buildId)
        complete(HttpEntity(
          `text/event-stream`,
          s"data:${Problem("BUILD-CHANGED").asJson.pretty(CompactPrinter)}\n\n"))  // Exact this message is checked in experimental GUI
      else
        eventDirective(eventWatch.lastAddedEventId, defaultTimeout = DefaultJsonSeqChunkTimeout) {
          case request: EventRequest[E] ⇒
            optionalHeaderValueByType[`Last-Event-ID`](()) { lastEventIdHeader ⇒
              val req = lastEventIdHeader.fold(request)(header ⇒
                request.copy[E](after = toLastEventId(header)))
              val mutableJsonPrinter = CompactPrinter.copy(reuseWriters = true)
              val source = eventWatch.observe(req, predicate = isRelevantEvent)
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

  private def eventDirective(
    defaultAfter: EventId,
    defaultTimeout: FiniteDuration = EventDirectives.DefaultTimeout,
    defaultDelay: FiniteDuration = EventDirectives.DefaultDelay)
  =
    new Directive1[SomeEventRequest[E]] {
      def tapply(inner: Tuple1[SomeEventRequest[E]] ⇒ Route) =
        eventRequest[E](
          defaultAfter = Some(defaultAfter),
          defaultDelay = defaultDelay,
          defaultTimeout = defaultTimeout,
          defaultReturnType = Some("Event"))
        .apply(eventRequest ⇒ inner(Tuple1(eventRequest)))
    }
}

object AbstractEventRoute
{
  private val DefaultJsonSeqChunkTimeout = 24.hours  // Renewed for each chunked

  private def toLastEventId(header: `Last-Event-ID`): EventId =
    try java.lang.Long.parseLong(header.id)
    catch {
      case e: NumberFormatException ⇒
        throw new HttpStatusCodeException(BadRequest, Problem(s"Invalid header Last-Event-Id: $e"))
    }
}
