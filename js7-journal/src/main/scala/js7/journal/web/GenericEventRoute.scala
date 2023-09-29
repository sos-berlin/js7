package js7.journal.web

import akka.NotUsed
import akka.actor.ActorRefFactory
import akka.http.scaladsl.common.JsonEntityStreamingSupport
import akka.http.scaladsl.marshalling.{ToEntityMarshaller, ToResponseMarshallable}
import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.StatusCodes.{BadRequest, ServiceUnavailable}
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.{Directive, Directive1, ExceptionHandler, Route}
import akka.stream.scaladsl.Source
import akka.util.ByteString
import izumi.reflect.Tag
import js7.base.auth.{UserId, ValidUserPermission}
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax.RichMonixObservable
import js7.base.problem.Problem
import js7.base.problem.Problems.ShuttingDownProblem
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.time.ScalaTime.*
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.FutureCompletion
import js7.base.utils.FutureCompletion.syntax.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.akkahttp.AkkaHttpServerUtils.{accept, completeTask, observableToResponseMarshallable}
import js7.common.akkahttp.StandardMarshallers.*
import js7.common.akkahttp.web.session.RouteProvider
import js7.common.http.JsonStreamingSupport.*
import js7.data.event.JournalEvent.StampedHeartbeat
import js7.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, EventSeq, EventSeqTornProblem, KeyedEvent, KeyedEventTypedJsonCodec, Stamped, TearableEventSeq}
import js7.journal.watch.{ClosedException, EventWatch}
import js7.journal.web.EventDirectives.eventRequest
import js7.journal.web.GenericEventRoute.*
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import scala.concurrent.duration.*
import scala.concurrent.duration.Deadline.now
import scala.util.chaining.*
import scala.util.control.{NoStackTrace, NonFatal}

/**
  * @author Joacim Zschimmer
  */
trait GenericEventRoute extends RouteProvider:
  protected implicit def actorRefFactory: ActorRefFactory
  private implicit def implicitScheduler: Scheduler = scheduler
  protected def eventWatch: EventWatch

  private lazy val whenShuttingDownCompletion = new FutureCompletion(whenShuttingDown)
  private lazy val defaultJsonSeqChunkTimeout =
    config.getDuration("js7.web.server.services.event.streaming.chunk-timeout").toFiniteDuration
  private lazy val defaultStreamingDelay =
    config.getDuration("js7.web.server.services.event.streaming.delay").toFiniteDuration

  protected trait GenericEventRouteProvider:
    implicit protected def keyedEventTypedJsonCodec: KeyedEventTypedJsonCodec[Event]

    protected def isRelevantEvent(keyedEvent: KeyedEvent[Event]) = true

    protected def defaultReturnType: Option[Class[? <: Event]] = Some(classOf[Event])

    protected def filterObservable: StampedEventFilter =
      identity

    implicit private val exceptionHandler: ExceptionHandler =
      ExceptionHandler:
        case t: ClosedException if t.getMessage != null =>
          if whenShuttingDown.isCompleted then
            complete(ServiceUnavailable -> ShuttingDownProblem)
          else
            complete(ServiceUnavailable -> Problem.pure(t.getMessage))
        //case t: akka.pattern.AskTimeoutException =>  // When getting EventWatch (Actor maybe terminated)
        //  logger.debug(t.toStringWithCauses, t)
        //  complete(ServiceUnavailable -> Problem.pure(t.toString))

    final lazy val route: Route =
      get:
        pathEnd:
          accept(`application/x-ndjson`):
            Route.seal:
              authorizedUser(ValidUserPermission) { user =>
                implicit val userId = user.id
                val waitingSince = !eventWatch.whenStarted.isCompleted ? now
                if waitingSince.isDefined then logger.debug("Waiting for journal to become ready ...")
                onSuccess(eventWatch.whenStarted) { eventWatch =>
                  for o <- waitingSince do logger.debug("Journal has become ready after " +
                    o.elapsed.pretty + ", continuing event web service")
                  implicit val s = NdJsonStreamingSupport
                  Route.seal(
                    jsonSeqEvents(eventWatch))
                }
              }

    private def jsonSeqEvents(eventWatch: EventWatch)
      (implicit userId: UserId, s: JsonEntityStreamingSupport): Route =
      parameter("onlyAcks" ? false) { onlyAcks =>
        parameter("heartbeat".as[FiniteDuration].?) { maybeHeartbeat =>  // Echo last EventId as a heartbeat
          if onlyAcks then
            eventIdRoute(maybeHeartbeat, eventWatch)
          else
            eventDirective(
              eventWatch.lastAddedEventId,
              defaultTimeout = defaultJsonSeqChunkTimeout,
              defaultDelay = defaultStreamingDelay
            ) { request =>
              eventRoute(request, maybeHeartbeat, eventWatch)
            }
        }
      }

    private def eventIdRoute(maybeHeartbeat: Option[FiniteDuration], eventWatch: EventWatch)
      (implicit s: JsonEntityStreamingSupport)
    : Route =
      parameter("timeout" ? defaultJsonSeqChunkTimeout) { timeout =>
        implicit val x: ToEntityMarshaller[EventId] = jsonSeqMarshaller

        completeTask[ToResponseMarshallable](
          eventWatch
            .observeEventIds(Some(timeout))
            .map(_.map(observable =>
              observableToMarshallable(
                observable
                  .pipe(o => maybeHeartbeat.fold(o)(o.echoRepeated))))))
      }

    private def eventRoute(
      request: EventRequest[Event],
      maybeHeartbeat: Option[FiniteDuration],
      eventWatch: EventWatch)
      (implicit userId: UserId, s: JsonEntityStreamingSupport)
    : Route =
      extractRequest { httpRequest =>
        val toResponseMarshallable: Task[ToResponseMarshallable] =
          maybeHeartbeat match
            case None =>
              // Await the first event to check for Torn and convert it to a proper error message, otherwise continue with observe
              awaitFirstEventObservable(request, eventWatch, httpRequest)

            case Some(heartbeat) =>
              Task(
                observableToResponseMarshallable(
                  heartbeatingObservable(request, heartbeat, eventWatch),
                  httpRequest, userId, whenShuttingDownCompletion, chunkSize = chunkSize))

        complete(
          toResponseMarshallable
            .onCancelRaiseError(CanceledException)
            .onErrorRecover {
              case CanceledException => emptyResponseMarshallable
            }
            .runToFuture
            .cancelOnCompletionOf(whenShuttingDownCompletion))
      }

    private def awaitFirstEventObservable(
      request: EventRequest[Event],
      eventWatch: EventWatch,
      httpRequest: HttpRequest)
      (implicit userId: UserId, s: JsonEntityStreamingSupport)
    : Task[ToResponseMarshallable] =
      Task.defer:
        val runningSince = now
        val initialRequest = request.copy[Event](
          limit = 1 min request.limit)
        eventWatch
          .when(initialRequest, isRelevantEvent)
          .map:
            case TearableEventSeq.Torn(eventId) =>
              ToResponseMarshallable(
                BadRequest -> EventSeqTornProblem(requestedAfter = request.after, tornEventId = eventId))

            case EventSeq.Empty(_) =>
              emptyResponseMarshallable

            case EventSeq.NonEmpty(closeableIterator) =>
              val head = autoClosing(closeableIterator)(_.next())

              val tailRequest = request.copy[Event](
                after = head.eventId,
                limit = request.limit - 1,
                delay = (request.delay - runningSince.elapsed) min ZeroDuration)

              observableToResponseMarshallable(
                head +: eventObservable(tailRequest, isRelevantEvent, eventWatch),
                httpRequest, userId, whenShuttingDownCompletion, chunkSize = chunkSize)

    private def emptyResponseMarshallable(implicit s: JsonEntityStreamingSupport)
    : ToResponseMarshallable =
      implicit val x: ToEntityMarshaller[Unit] = jsonSeqMarshaller
      observableToMarshallable(
        Observable.empty[Unit])

    private def heartbeatingObservable(
      request: EventRequest[Event],
      heartbeat: FiniteDuration,
      eventWatch: EventWatch)
    : Observable[Stamped[AnyKeyedEvent]] =
      // TODO Check if torn then return Task.raiseError
      StampedHeartbeat +:
        eventObservable(request, isRelevantEvent, eventWatch)
          .insertHeartbeatsOnSlowUpstream(heartbeat, StampedHeartbeat)

    private def eventObservable(
      request: EventRequest[Event],
      predicate: AnyKeyedEvent => Boolean,
      eventWatch: EventWatch)
    : Observable[Stamped[AnyKeyedEvent]] =
      filterObservable(
        eventWatch  // Continue with an Observable, skipping the already read event
          .observe(request, predicate)
      ) .onErrorRecoverWith { case NonFatal(e) =>
          logger.warn(e.toStringWithCauses)
          if e.getStackTrace.nonEmpty then logger.debug(e.toStringWithCauses, e)
          Observable.empty  // The streaming event web service doesn't have an error channel, so we simply end the tail
        }

    private def eventDirective(
      defaultAfter: EventId,
      defaultTimeout: FiniteDuration = EventDirectives.DefaultTimeout,
      defaultDelay: FiniteDuration = EventDirectives.DefaultDelay)
    : Directive1[EventRequest[Event]] =
      Directive(inner =>
        eventRequest[Event](
          defaultAfter = Some(defaultAfter),
          defaultDelay = defaultDelay,
          defaultTimeout = defaultTimeout,
          defaultReturnType = defaultReturnType.map(_.simpleScalaName))
        .apply(eventRequest => inner(Tuple1(eventRequest))))

  private def observableToMarshallable[A: Tag](observable: Observable[A])
    (implicit q: Source[A, NotUsed] => ToResponseMarshallable)
  : ToResponseMarshallable =
    monixObservableToMarshallable(
      observable.takeUntilCompletedAndDo(whenShuttingDownCompletion)(_ =>
        Task { logger.debug("whenShuttingDown completed") }))

object GenericEventRoute:
  type StampedEventFilter =
    Observable[Stamped[KeyedEvent[Event]]] =>
      Observable[Stamped[KeyedEvent[Event]]]

  private val logger = Logger[this.type]
  private val LF = ByteString("\n")

  private object CanceledException extends Exception with NoStackTrace
