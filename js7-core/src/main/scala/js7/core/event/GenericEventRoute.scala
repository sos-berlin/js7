package js7.core.event

import akka.NotUsed
import akka.http.scaladsl.common.JsonEntityStreamingSupport
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.marshalling.sse.EventStreamMarshalling._
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.MediaTypes.`text/event-stream`
import akka.http.scaladsl.model.StatusCodes.{BadRequest, ServiceUnavailable}
import akka.http.scaladsl.model.headers.`Last-Event-ID`
import akka.http.scaladsl.model.sse.ServerSentEvent
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive1, ExceptionHandler, Route}
import akka.stream.scaladsl.Source
import js7.base.BuildInfo
import js7.base.auth.ValidUserPermission
import js7.base.circeutils.CirceUtils.{CompactPrinter, RichJson}
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime._
import js7.base.utils.Assertions.assertThat
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.ScalaUtils.{RichJavaClass, RichThrowable, _}
import js7.base.utils.ScalazStyle._
import js7.common.akkahttp.AkkaHttpServerUtils.{accept, completeTask}
import js7.common.akkahttp.CirceJsonOrYamlSupport.jsonOrYamlMarshaller
import js7.common.akkahttp.EventSeqStreamingSupport.NonEmptyEventSeqJsonStreamingSupport
import js7.common.akkahttp.HttpStatusCodeException
import js7.common.akkahttp.StandardDirectives.routeTask
import js7.common.akkahttp.StandardMarshallers._
import js7.common.akkahttp.html.HtmlDirectives.htmlPreferred
import js7.common.akkahttp.web.session.RouteProvider
import js7.common.event.EventWatch
import js7.common.event.collector.EventDirectives
import js7.common.event.collector.EventDirectives.eventRequest
import js7.common.http.JsonStreamingSupport._
import js7.common.scalautil.Logger
import js7.common.time.JavaTimeConverters.AsScalaDuration
import js7.core.event.GenericEventRoute._
import js7.core.problems.JobSchedulerIsShuttingDownProblem
import js7.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, EventSeq, EventSeqTornProblem, KeyedEvent, KeyedEventTypedJsonCodec, Stamped, TearableEventSeq}
import io.circe.syntax.EncoderOps
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._
import scala.reflect.runtime.universe._
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
trait GenericEventRoute extends RouteProvider
{
  protected def isShuttingDown: Boolean

  private implicit def implicitScheduler: Scheduler = scheduler

  private lazy val defaultJsonSeqChunkTimeout = config.getDuration("js7.webserver.services.event.streaming.chunk-timeout")
    .toFiniteDuration
  private lazy val defaultStreamingDelay = config.getDuration("js7.webserver.services.event.streaming.delay")
    .toFiniteDuration

  protected trait GenericEventRouteProvider
  {
    implicit protected def keyedEventTypedJsonCodec: KeyedEventTypedJsonCodec[Event]

    protected def eventWatchFor(user: Session#User): Task[Checked[EventWatch]]

    protected def isRelevantEvent(keyedEvent: KeyedEvent[Event]) = true

    protected def defaultReturnType: Option[Class[_ <: Event]] = Some(classOf[Event])

    private val exceptionHandler = ExceptionHandler {
      case t: js7.core.event.journal.watch.ClosedException if t.getMessage != null =>
        if (isShuttingDown)
          complete(ServiceUnavailable -> JobSchedulerIsShuttingDownProblem)
        else
          complete(ServiceUnavailable -> Problem.pure(t.getMessage))
      case t: akka.pattern.AskTimeoutException =>  // When getting EventWatch (Actor maybe terminated)
        logger.debug(t.toStringWithCauses, t)
        complete(ServiceUnavailable -> Problem.pure(t.toString))
    }

    final lazy val route: Route =
      get {
        pathEnd {
          authorizedUser(ValidUserPermission) { user =>
            handleExceptions(exceptionHandler) {
              routeTask(
                eventWatchFor(user)/*⚡️AkkaAskTimeout*/ map {
                  case Left(problem) =>
                    complete(problem)

                  case Right(eventWatch) =>
                    val waitingSince = !eventWatch.whenStarted.isCompleted ? now
                    if (waitingSince.isDefined) logger.debug("Waiting for journal to become ready ...")
                    onSuccess(eventWatch.whenStarted) { eventWatch =>
                      for (o <- waitingSince) {
                        logger.debug(s"Journal has become ready after ${o.elapsed.pretty}, continuing event web service")
                      }
                      htmlPreferred {
                        oneShot(eventWatch)
                      } ~
                      accept(`application/x-ndjson`) {
                        jsonSeqEvents(eventWatch)(NdJsonStreamingSupport)
                      } ~
                      accept(`application/json-seq`) {
                        jsonSeqEvents(eventWatch)(JsonSeqStreamingSupport)
                      } ~
                      accept(`text/event-stream`) {
                        serverSentEvents(eventWatch)
                      } ~
                      oneShot(eventWatch)
                    }
                })
            }
          }
        }
      }

    private def oneShot(eventWatch: EventWatch): Route =
      eventDirective(eventWatch.lastAddedEventId) { request =>
        intelliJuseImport(jsonOrYamlMarshaller)
        completeTask(
          eventWatch.when[Event](request, predicate = isRelevantEvent).map {
            case o: TearableEventSeq.Torn =>
              ToResponseMarshallable(o: TearableEventSeq[Seq, KeyedEvent[Event]])

            case o: EventSeq.Empty =>
              ToResponseMarshallable(o: TearableEventSeq[Seq, KeyedEvent[Event]])

            case EventSeq.NonEmpty(events) =>
              implicit val x = NonEmptyEventSeqJsonStreamingSupport
              closeableIteratorToMarshallable(events.takeWhile(_ => !isShuttingDown))
          })
      }

    private def jsonSeqEvents(eventWatch: EventWatch)(implicit streamingSupport: JsonEntityStreamingSupport): Route =
      eventDirective(eventWatch.lastAddedEventId, defaultTimeout = defaultJsonSeqChunkTimeout, defaultDelay = defaultStreamingDelay) { request =>
        parameter("eventIdOnly" ? false) { eventIdOnly =>
          parameter("heartbeat".as[FiniteDuration].?) { maybeHeartbeat =>  // Echo last EventId as a heartbeat
            def predicate(ke: KeyedEvent[Event]) = eventIdOnly || isRelevantEvent(ke)
            parameter("onlyLastOfChunk" ? false) { onlyLastOfChunk =>
              if (maybeHeartbeat.isDefined && !eventIdOnly)
                complete(BadRequest -> Problem.pure("heartbeat=... is allowed only in conjunction with eventIdOnly=true"))
              else {
                val runningSince = now
                val initialRequest = request.copy[Event](
                  limit = 1 min request.limit,
                  timeout = maybeHeartbeat.fold(request.timeout)(heartbeat => Some(request.timeout.fold(heartbeat)(heartbeat.min))))

                completeTask(
                  // Await the first event to check for Torn and convert it to a proper error message, otherwise continue with observe
                  eventWatch.when(initialRequest, predicate) map {
                    case TearableEventSeq.Torn(eventId) =>
                      ToResponseMarshallable(
                        BadRequest -> EventSeqTornProblem(requestedAfter = request.after, tornEventId = eventId))

                    case EventSeq.Empty(_) =>
                      maybeHeartbeat match {
                        case None =>
                          implicit val x = jsonSeqMarshaller[Unit]
                          observableToMarshallable(
                            Observable.empty[Unit])
                        case Some(heartbeat) =>  // No event arrived until first heartbeat
                          assertThat(eventIdOnly)
                          implicit val x = jsonSeqMarshaller[EventId]
                          observableToMarshallable(
                            (eventWatch.lastAddedEventId/*start heartbeating after this immediately returned value*/ +:
                              observe(request, predicate, onlyLastOfChunk, eventWatch).map(_.eventId)
                            ).echoRepeated(heartbeat))
                      }

                    case EventSeq.NonEmpty(closeableIterator) =>
                      val head = autoClosing(closeableIterator)(_.next())
                      val tail = observe(  // Continue with an Observable, skipping the already read event
                        request.copy[Event](
                          after = head.eventId,
                          limit = request.limit - 1,
                          delay = (request.delay - runningSince.elapsed) min Duration.Zero),
                        predicate,
                        onlyLastOfChunk,
                        eventWatch)
                      val observable = (head +: tail)
                      if (eventIdOnly) {
                        val eventIds = observable.map(_.eventId)
                        implicit val x = jsonSeqMarshaller[EventId]
                        observableToMarshallable(
                          eventIds.pipe(o => maybeHeartbeat.fold(o)(o.echoRepeated)))
                      } else {
                        implicit val x = jsonSeqMarshaller[Stamped[KeyedEvent[Event]]]
                        observableToMarshallable(observable)
                      }
                    })
              }
            }
          }
        }
      }

    private def observe(request: EventRequest[Event], predicate: AnyKeyedEvent => Boolean, onlyLastOfChunk: Boolean, eventWatch: EventWatch)
    : Observable[Stamped[AnyKeyedEvent]] =
      eventWatch  // Continue with an Observable, skipping the already read event
        .observe(
          request,
          predicate,
          onlyLastOfChunk = onlyLastOfChunk)
        .onErrorRecoverWith { case NonFatal(e) =>
          logger.warn(e.toStringWithCauses)
          Observable.empty  // The streaming event web service doesn't have an error channel, so we simply end the tail
        }

    private def serverSentEvents(eventWatch: EventWatch): Route =
      parameter("v" ? BuildInfo.buildId) { requestedBuildId =>
        if (requestedBuildId != BuildInfo.buildId)
          complete(HttpEntity(
            `text/event-stream`,
            s"data:${Problem("BUILD-CHANGED").asJson(Problem.typedJsonEncoder).printWith(CompactPrinter)}\n\n"))  // Exact this message is checked in experimental GUI
        else
          eventDirective(eventWatch.lastAddedEventId, defaultTimeout = defaultJsonSeqChunkTimeout) { request =>
            optionalHeaderValueByType[`Last-Event-ID`](()) { lastEventIdHeader =>
              val req = lastEventIdHeader.fold(request)(header =>
                request.copy[Event](after = toLastEventId(header)))
              complete(
                observableToMarshallable(
                  eventWatch.observe(req, predicate = isRelevantEvent)
                    .map(stamped => ServerSentEvent(
                      data = stamped.asJson.compactPrint,
                      id = Some(stamped.eventId.toString)))))
            }
          }
      }

    private def eventDirective(
      defaultAfter: EventId,
      defaultTimeout: FiniteDuration = EventDirectives.DefaultTimeout,
      defaultDelay: FiniteDuration = EventDirectives.DefaultDelay)
    =
      new Directive1[EventRequest[Event]] {
        def tapply(inner: Tuple1[EventRequest[Event]] => Route) =
          eventRequest[Event](
            defaultAfter = Some(defaultAfter),
            defaultDelay = defaultDelay,
            defaultTimeout = defaultTimeout,
            defaultReturnType = defaultReturnType map (_.simpleScalaName))
          .apply(eventRequest => inner(Tuple1(eventRequest)))
      }
  }

  private def observableToMarshallable[A: TypeTag](observable: Observable[A])
    (implicit q: Source[A, NotUsed] => ToResponseMarshallable)
  : ToResponseMarshallable =
    monixObservableToMarshallable(
      Observable.defer(
        if (isShuttingDown)
          Observable.empty
        else
          observable.takeWhile(_ => !isShuttingDown)))
}

object GenericEventRoute
{
  private val logger = Logger(getClass)

  private def toLastEventId(header: `Last-Event-ID`): EventId =
    try java.lang.Long.parseLong(header.id)
    catch {
      case e: NumberFormatException =>
        throw new HttpStatusCodeException(BadRequest, Problem.pure(s"Invalid header Last-Event-Id: $e"))
    }
}
