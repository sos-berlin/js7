package com.sos.jobscheduler.core.event

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.marshalling.sse.EventStreamMarshalling._
import akka.http.scaladsl.model.MediaTypes.`text/event-stream`
import akka.http.scaladsl.model.StatusCodes.BadRequest
import akka.http.scaladsl.model.headers.`Last-Event-ID`
import akka.http.scaladsl.model.sse.ServerSentEvent
import akka.http.scaladsl.model.{HttpEntity, StatusCodes}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive1, ExceptionHandler, Route}
import com.sos.jobscheduler.base.auth.ValidUserPermission
import com.sos.jobscheduler.base.circeutils.CirceUtils.CompactPrinter
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.time.Timestamp.now
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.base.utils.ScalaUtils.RichJavaClass
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
import com.sos.jobscheduler.core.event.GenericEventRoute._
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, EventSeq, KeyedEvent, KeyedEventTypedJsonCodec, Stamped, TearableEventSeq}
import io.circe.syntax.EncoderOps
import monix.eval.Task
import monix.reactive.Observable
import scala.collection.immutable.Seq
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
trait GenericEventRoute extends RouteProvider
{
  private implicit def implicitScheduler = scheduler

  protected trait GenericEventRouteProvider
  {
    implicit protected def keyedEventTypedJsonCodec: KeyedEventTypedJsonCodec[Event]

    protected def eventWatchFor(user: Session#User): Task[EventWatch[Event]]

    protected def isRelevantEvent(keyedEvent: KeyedEvent[Event]):Boolean = true

    protected def defaultReturnType: Option[Class[_ <: Event]] = Some(classOf[Event])

    private val exceptionHandler = ExceptionHandler {
      case t: com.sos.jobscheduler.core.event.journal.watch.ClosedException ⇒
        complete((StatusCodes.ServiceUnavailable, Problem.fromEager(t.getMessage)))
    }

    final val route: Route =
      get {
        pathEnd {
          authorizedUser(ValidUserPermission) { user ⇒
            handleExceptions(exceptionHandler) {
              routeFuture(
                eventWatchFor(user).runAsync.map { eventWatch ⇒
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
      }

    private def oneShot(eventWatch: EventWatch[Event]): Route =
      eventDirective(eventWatch.lastAddedEventId) { request ⇒
        intelliJuseImport(jsonOrYamlMarshaller)
        val marshallable = eventWatch.when[Event](request, predicate = isRelevantEvent) map {
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

    private def jsonSeqEvents(eventWatch: EventWatch[Event]): Route =
      eventDirective(eventWatch.lastAddedEventId, defaultTimeout = DefaultJsonSeqChunkTimeout, defaultDelay = Duration.Zero) { request ⇒
        implicit val x = JsonSeqStreamSupport
        implicit val y = jsonSeqMarshaller[Stamped[KeyedEvent[Event]]]
        val t = now
        complete(
          // Await the first event to check for Torn and convert it to a proper error message, otherwise continue with observe
          eventWatch.when(request, predicate = isRelevantEvent) map {
            case TearableEventSeq.Torn(eventId) ⇒
              Problem.fromEager(s"Requested EventId after=${request.after} is not available. Oldest available EventId is $eventId")
                : ToResponseMarshallable

            case EventSeq.Empty(_) ⇒
              Observable.empty[Stamped[KeyedEvent[Event]]]
                : ToResponseMarshallable

            case EventSeq.NonEmpty(iterator) ⇒
              val head = iterator.next()
              // Continue with an Observable, skipping the already read event
              val tail = eventWatch.observe(
                request = request.copy[Event](
                  after = head.eventId,
                  limit = request.limit - 1,
                  timeout = Duration.Zero,
                  delay = (request.delay - (now - t)) min Duration.Zero),
                predicate = isRelevantEvent)
              Observable(head) ++ tail
                : ToResponseMarshallable
            })
    }

    private def serverSentEvents(eventWatch: EventWatch[Event]): Route =
      parameter("v" ? BuildInfo.buildId) { requestedBuildId ⇒
        if (requestedBuildId != BuildInfo.buildId)
          complete(HttpEntity(
            `text/event-stream`,
            s"data:${Problem("BUILD-CHANGED").asJson.pretty(CompactPrinter)}\n\n"))  // Exact this message is checked in experimental GUI
        else
          eventDirective(eventWatch.lastAddedEventId, defaultTimeout = DefaultJsonSeqChunkTimeout) { request ⇒
            optionalHeaderValueByType[`Last-Event-ID`](()) { lastEventIdHeader ⇒
              val req = lastEventIdHeader.fold(request)(header ⇒
                request.copy[Event](after = toLastEventId(header)))
              val mutableJsonPrinter = CompactPrinter.copy(reuseWriters = true)
              val source = eventWatch.observe(req, predicate = isRelevantEvent)
                .map(stamped ⇒ ServerSentEvent(
                  data = stamped.asJson.pretty(mutableJsonPrinter),
                  id = Some(stamped.eventId.toString)))
                .toAkkaSource
              complete(source)
            }
          }
      }

    private def eventDirective(
      defaultAfter: EventId,
      defaultTimeout: FiniteDuration = EventDirectives.DefaultTimeout,
      defaultDelay: FiniteDuration = EventDirectives.DefaultDelay)
    =
      new Directive1[EventRequest[Event]] {
        def tapply(inner: Tuple1[EventRequest[Event]] ⇒ Route) =
          eventRequest[Event](
            defaultAfter = Some(defaultAfter),
            defaultDelay = defaultDelay,
            defaultTimeout = defaultTimeout,
            defaultReturnType = defaultReturnType map (_.simpleScalaName))
          .apply(eventRequest ⇒ inner(Tuple1(eventRequest)))
      }
  }
}

object GenericEventRoute
{
  private val DefaultJsonSeqChunkTimeout = 24.hours  // Renewed for each chunk

  private def toLastEventId(header: `Last-Event-ID`): EventId =
    try java.lang.Long.parseLong(header.id)
    catch {
      case e: NumberFormatException ⇒
        throw new HttpStatusCodeException(BadRequest, Problem(s"Invalid header Last-Event-Id: $e"))
    }
}
