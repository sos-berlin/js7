package com.sos.jobscheduler.core.event

import akka.http.scaladsl.common.JsonEntityStreamingSupport
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
import com.sos.jobscheduler.base.utils.ScalaUtils.{RichJavaClass, RichThrowable}
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.{accept, completeTask}
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport.jsonOrYamlMarshaller
import com.sos.jobscheduler.common.akkahttp.EventSeqStreamingSupport.NonEmptyEventSeqJsonStreamingSupport
import com.sos.jobscheduler.common.akkahttp.HttpStatusCodeException
import com.sos.jobscheduler.common.akkahttp.JsonStreamingSupport._
import com.sos.jobscheduler.common.akkahttp.StandardDirectives.routeTask
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import com.sos.jobscheduler.common.akkahttp.StreamingSupport._
import com.sos.jobscheduler.common.akkahttp.html.HtmlDirectives.htmlPreferred
import com.sos.jobscheduler.common.akkahttp.web.session.RouteProvider
import com.sos.jobscheduler.common.event.EventWatch
import com.sos.jobscheduler.common.event.collector.EventDirectives
import com.sos.jobscheduler.common.event.collector.EventDirectives.eventRequest
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.event.GenericEventRoute._
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, EventSeq, EventSeqTornProblem, KeyedEvent, KeyedEventTypedJsonCodec, Stamped, TearableEventSeq}
import io.circe.syntax.EncoderOps
import monix.eval.Task
import monix.reactive.Observable
import scala.collection.immutable.Seq
import scala.concurrent.duration._
import scala.util.control.NonFatal

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

    protected def isRelevantEvent(keyedEvent: KeyedEvent[Event]) = true

    protected def defaultReturnType: Option[Class[_ <: Event]] = Some(classOf[Event])

    private val exceptionHandler = ExceptionHandler {
      case t: com.sos.jobscheduler.core.event.journal.watch.ClosedException ⇒
        complete((StatusCodes.ServiceUnavailable, Problem.pure(t.getMessage)))
    }

    final lazy val route: Route =
      get {
        pathEnd {
          handleExceptions(exceptionHandler) {
            authorizedUser(ValidUserPermission) { user ⇒
              routeTask(
                eventWatchFor(user)/*⚡️AkkaAskTimeout*/ map { eventWatch ⇒
                  htmlPreferred {
                    oneShot(eventWatch)
                  } ~
                  accept(`application/x-ndjson`) {
                    jsonSeqEvents(eventWatch, NdJsonStreamingSupport)
                  } ~
                  accept(`application/json-seq`) {
                    jsonSeqEvents(eventWatch, JsonSeqStreamingSupport)
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
        completeTask(
          eventWatch.when[Event](request, predicate = isRelevantEvent).map {
            case o: TearableEventSeq.Torn ⇒
              ToResponseMarshallable(o: TearableEventSeq[Seq, KeyedEvent[Event]])

            case o: EventSeq.Empty ⇒
              ToResponseMarshallable(o: TearableEventSeq[Seq, KeyedEvent[Event]])

            case EventSeq.NonEmpty(events) ⇒
              implicit val x = NonEmptyEventSeqJsonStreamingSupport
              closeableIteratorToMarshallable(events)
          })
      }

    private def jsonSeqEvents(eventWatch: EventWatch[Event], streamingSupport: JsonEntityStreamingSupport): Route =
      eventDirective(eventWatch.lastAddedEventId, defaultTimeout = DefaultJsonSeqChunkTimeout, defaultDelay = Duration.Zero) { request ⇒
        implicit val x = streamingSupport
        implicit val y = jsonSeqMarshaller[Stamped[KeyedEvent[Event]]]
        val t = now
        completeTask(
          // Await the first event to check for Torn and convert it to a proper error message, otherwise continue with observe
          eventWatch.when(request, predicate = isRelevantEvent).map {
            case TearableEventSeq.Torn(eventId) ⇒
              EventSeqTornProblem(requestedAfter = request.after, tornEventId = eventId): ToResponseMarshallable

            case EventSeq.Empty(_) ⇒
              monixObservableToMarshallable(
                Observable.empty[Stamped[KeyedEvent[Event]]])

            case EventSeq.NonEmpty(closeableIterator) ⇒
              val head = autoClosing(closeableIterator)(_.next())
              val tail = eventWatch  // Continue with an Observable, skipping the already read event
                .observe(
                  request = request.copy[Event](
                    after = head.eventId,
                    limit = request.limit - 1,
                    delay = (request.delay - (now - t)) min Duration.Zero),
                  predicate = isRelevantEvent)
                .onErrorRecoverWith { case NonFatal(e) ⇒
                  logger.warn(e.toStringWithCauses)
                  Observable.empty  // The streaming event web service doesn't have an error channel, so we simply end the tail
                }
              monixObservableToMarshallable(
                Observable.pure(head) ++ tail)
            })
    }

    private def serverSentEvents(eventWatch: EventWatch[Event]): Route =
      parameter("v" ? BuildInfo.buildId) { requestedBuildId ⇒
        if (requestedBuildId != BuildInfo.buildId)
          complete(HttpEntity(
            `text/event-stream`,
            s"data:${Problem("BUILD-CHANGED").asJson(Problem.typedJsonEncoder).pretty(CompactPrinter)}\n\n"))  // Exact this message is checked in experimental GUI
        else
          eventDirective(eventWatch.lastAddedEventId, defaultTimeout = DefaultJsonSeqChunkTimeout) { request ⇒
            optionalHeaderValueByType[`Last-Event-ID`](()) { lastEventIdHeader ⇒
              val req = lastEventIdHeader.fold(request)(header ⇒
                request.copy[Event](after = toLastEventId(header)))
              val mutableJsonPrinter = CompactPrinter.copy(reuseWriters = true)
              val source = logErrorToWebLog(
                eventWatch.observe(req, predicate = isRelevantEvent)
                  .map(stamped ⇒ ServerSentEvent(
                    data = stamped.asJson.pretty(mutableJsonPrinter),
                    id = Some(stamped.eventId.toString)))
                  .toAkkaSource)
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
  private val logger = Logger(getClass)
  private val DefaultJsonSeqChunkTimeout = 24.hours  // Renewed for each chunk

  private def toLastEventId(header: `Last-Event-ID`): EventId =
    try java.lang.Long.parseLong(header.id)
    catch {
      case e: NumberFormatException ⇒
        throw new HttpStatusCodeException(BadRequest, Problem.pure(s"Invalid header Last-Event-Id: $e"))
    }
}
