package js7.journal.web

import akka.NotUsed
import akka.actor.ActorRefFactory
import akka.http.scaladsl.common.JsonEntityStreamingSupport
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.StatusCodes.{BadRequest, ServiceUnavailable}
import akka.http.scaladsl.model.headers.`Last-Event-ID`
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive, Directive1, ExceptionHandler, Route}
import akka.stream.scaladsl.Source
import io.circe.syntax.EncoderOps
import js7.base.auth.ValidUserPermission
import js7.base.circeutils.CirceUtils.{CompactPrinter, RichJson}
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.closeableIteratorToObservable
import js7.base.problem.Problems.ShuttingDownProblem
import js7.base.problem.{Checked, Problem}
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.time.ScalaTime._
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.FutureCompletion
import js7.base.utils.FutureCompletion.syntax._
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.ScalaUtils.syntax._
import js7.common.akkahttp.AkkaHttpServerUtils.{accept, completeTask}
import js7.common.akkahttp.CirceJsonOrYamlSupport.jsonOrYamlMarshaller
import js7.common.akkahttp.EventSeqStreamingSupport.NonEmptyEventSeqJsonStreamingSupport
import js7.common.akkahttp.HttpStatusCodeException
import js7.common.akkahttp.StandardDirectives.routeTask
import js7.common.akkahttp.StandardMarshallers._
import js7.common.akkahttp.html.HtmlDirectives.htmlPreferred
import js7.common.akkahttp.web.session.RouteProvider
import js7.common.http.JsonStreamingSupport._
import js7.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, EventSeq, EventSeqTornProblem, KeyedEvent, KeyedEventTypedJsonCodec, Stamped, TearableEventSeq}
import js7.journal.watch.{ClosedException, EventWatch}
import js7.journal.web.EventDirectives.eventRequest
import js7.journal.web.GenericEventRoute._
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._
import scala.reflect.runtime.universe._
import scala.util.chaining._
import scala.util.control.{NoStackTrace, NonFatal}

/**
  * @author Joacim Zschimmer
  */
trait GenericEventRoute extends RouteProvider
{
  protected implicit def actorRefFactory: ActorRefFactory
  private implicit def implicitScheduler: Scheduler = scheduler

  private lazy val whenShuttingDownCompletion = new FutureCompletion(whenShuttingDown)
  private lazy val defaultJsonSeqChunkTimeout = config.getDuration("js7.web.server.services.event.streaming.chunk-timeout")
    .toFiniteDuration
  private lazy val defaultStreamingDelay = config.getDuration("js7.web.server.services.event.streaming.delay")
    .toFiniteDuration

  protected trait GenericEventRouteProvider
  {
    implicit protected def keyedEventTypedJsonCodec: KeyedEventTypedJsonCodec[Event]

    protected def eventWatchFor(user: Session#User): Task[Checked[EventWatch]]

    protected def isRelevantEvent(keyedEvent: KeyedEvent[Event]) = true

    protected def defaultReturnType: Option[Class[_ <: Event]] = Some(classOf[Event])

    protected def filterObservable: StampedEventFilter =
      identity

    implicit private val exceptionHandler = ExceptionHandler {
      case t: ClosedException if t.getMessage != null =>
        if (whenShuttingDown.isCompleted)
          complete(ServiceUnavailable -> ShuttingDownProblem)
        else
          complete(ServiceUnavailable -> Problem.pure(t.getMessage))
      //case t: akka.pattern.AskTimeoutException =>  // When getting EventWatch (Actor maybe terminated)
      //  logger.debug(t.toStringWithCauses, t)
      //  complete(ServiceUnavailable -> Problem.pure(t.toString))
    }

    final lazy val route: Route =
      get {
        pathEnd {
          Route.seal {
            authorizedUser(ValidUserPermission) { user =>
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
                        Route.seal(
                          oneShot(eventWatch))
                      } ~
                      accept(`application/x-ndjson`) {
                        Route.seal(
                          jsonSeqEvents(eventWatch)(NdJsonStreamingSupport))
                      } ~
                      Route.seal(
                        oneShot(eventWatch))
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
              observableToMarshallable(
                closeableIteratorToObservable(events))
          })
      }

    private def jsonSeqEvents(eventWatch: EventWatch)(implicit streamingSupport: JsonEntityStreamingSupport): Route =
      parameter("onlyAcks" ? false) { onlyAcks =>
        parameter("heartbeat".as[FiniteDuration].?) { maybeHeartbeat =>  // Echo last EventId as a heartbeat
          if (maybeHeartbeat.isDefined && !onlyAcks)
            complete(BadRequest -> Problem.pure("heartbeat=... is allowed only in conjunction with onlyAcks=true"))
          else if (onlyAcks)
            eventIdRoute(maybeHeartbeat, eventWatch)
          else
            eventDirective(eventWatch.lastAddedEventId, defaultTimeout = defaultJsonSeqChunkTimeout, defaultDelay = defaultStreamingDelay) { request =>
              eventRoute(request, eventWatch)
            }
        }
      }

    private def eventRoute(request: EventRequest[Event], eventWatch: EventWatch)
      (implicit streamingSupport: JsonEntityStreamingSupport)
    : Route = {
      def predicate(ke: KeyedEvent[Event]) = isRelevantEvent(ke)
      val runningSince = now
      val initialRequest = request.copy[Event](
        limit = 1 min request.limit)
      // Await the first event to check for Torn and convert it to a proper error message, otherwise continue with observe
      val future = eventWatch.when(initialRequest, predicate)
        .map {
          case TearableEventSeq.Torn(eventId) =>
            ToResponseMarshallable(
              BadRequest -> EventSeqTornProblem(requestedAfter = request.after, tornEventId = eventId))

          case EventSeq.Empty(_) =>
            emptyResponseMarshallable

          case EventSeq.NonEmpty(closeableIterator) =>
            val head = autoClosing(closeableIterator)(_.next())
              val tail = observe(  // Continue with an Observable, skipping the already read event
                request.copy[Event](
                  after = head.eventId,
                  limit = request.limit - 1,
                  delay = (request.delay - runningSince.elapsed) min Duration.Zero),
                predicate,
                eventWatch)
              implicit val x = jsonSeqMarshaller[Stamped[KeyedEvent[Event]]]
              observableToMarshallable(head +: tail)
        }
        .onCancelRaiseError(CanceledException)
        .onErrorRecover {
          case CanceledException => emptyResponseMarshallable
        }
        .runToFuture
        .cancelOnCompletionOf(whenShuttingDownCompletion)
      onSuccess(future)(complete(_))
    }

    private def emptyResponseMarshallable(implicit streamingSupport: JsonEntityStreamingSupport)
    : ToResponseMarshallable = {
      implicit val x = jsonSeqMarshaller[Unit]
      observableToMarshallable(
        Observable.empty[Unit])
    }

    private def eventIdRoute(maybeHeartbeat: Option[FiniteDuration], eventWatch: EventWatch)
      (implicit streamingSupport: JsonEntityStreamingSupport)
    : Route =
      parameter("timeout" ? defaultJsonSeqChunkTimeout) { timeout =>
        val maybeTimeout = timeout match {
          case o: FiniteDuration => Some(o)
          case _/*Duration.Inf only*/ => None
        }
        complete {
          implicit val x = jsonSeqMarshaller[EventId]
          observableToMarshallable(
            eventWatch.observeEventIds(maybeTimeout).pipe(o => maybeHeartbeat.fold(o)(o.echoRepeated)))
        }
      }

    private def observe(request: EventRequest[Event], predicate: AnyKeyedEvent => Boolean, eventWatch: EventWatch)
    : Observable[Stamped[AnyKeyedEvent]] =
      filterObservable(
        eventWatch  // Continue with an Observable, skipping the already read event
          .observe(request, predicate)
      ) .onErrorRecoverWith { case NonFatal(e) =>
          logger.warn(e.toStringWithCauses)
          if (e.getStackTrace.nonEmpty) logger.debug(e.toStringWithCauses, e)
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
  }

  private def observableToMarshallable[A: TypeTag](observable: Observable[A])
    (implicit q: Source[A, NotUsed] => ToResponseMarshallable)
  : ToResponseMarshallable =
    monixObservableToMarshallable(
      observable.takeUntilCompletedAndDo(whenShuttingDownCompletion)(_ =>
        Task { logger.debug("whenShuttingDown completed") }))
}

object GenericEventRoute
{
  type StampedEventFilter = Observable[Stamped[KeyedEvent[Event]]] => Observable[Stamped[KeyedEvent[Event]]]

  private val logger = Logger(getClass)

  private def toLastEventId(header: `Last-Event-ID`): EventId =
    try java.lang.Long.parseLong(header.id)
    catch {
      case e: NumberFormatException =>
        throw new HttpStatusCodeException(BadRequest, Problem.pure(s"Invalid header Last-Event-Id: $e"))
    }

  private object CanceledException extends Exception with NoStackTrace
}
