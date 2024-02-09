package js7.journal.web

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.syntax.applicativeError.*
import fs2.Stream
import izumi.reflect.Tag
import js7.base.auth.ValidUserPermission
import js7.base.fs2utils.StreamExtensions.*
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.problem.Problems.ShuttingDownProblem
import js7.base.problem.{Checked, Problem}
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.time.ScalaTime.*
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.http.JsonStreamingSupport.*
import js7.common.pekkohttp.PekkoHttpServerUtils.{accept, completeWithCheckedStream, encodeAndHeartbeatStream}
import js7.common.pekkohttp.StandardDirectives
import js7.common.pekkohttp.StandardDirectives.ioRoute
import js7.common.pekkohttp.StandardMarshallers.*
import js7.common.pekkohttp.web.session.RouteProvider
import js7.data.event.JournalEvent.{StampedHeartbeat, StampedHeartbeatIO}
import js7.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, EventSeq, EventSeqTornProblem, KeyedEvent, KeyedEventTypedJsonCodec, Stamped, TearableEventSeq}
import js7.journal.watch.{ClosedException, EventWatch}
import js7.journal.web.EventDirectives.eventRequest
import js7.journal.web.GenericEventRoute.*
import org.apache.pekko.actor.ActorRefFactory
import org.apache.pekko.http.scaladsl.model.StatusCodes.ServiceUnavailable
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.{Directive, Directive1, ExceptionHandler, Route}
import org.apache.pekko.util.ByteString
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.concurrent.duration.Deadline.now
import scala.util.chaining.*
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
trait GenericEventRoute extends RouteProvider:
  protected implicit def actorRefFactory: ActorRefFactory
  protected def eventWatch: EventWatch

  private given IORuntime = ioRuntime
  private given ExecutionContext = ioRuntime.compute

  private lazy val defaultJsonSeqChunkTimeout =
    config.getDuration("js7.web.server.services.event.streaming.chunk-timeout").toFiniteDuration
  private lazy val defaultStreamingDelay =
    config.getDuration("js7.web.server.services.event.streaming.delay").toFiniteDuration

  protected trait GenericEventRouteProvider:
    implicit protected def keyedEventTypedJsonCodec: KeyedEventTypedJsonCodec[Event]

    protected def isRelevantEvent(keyedEvent: KeyedEvent[Event]) = true

    protected def defaultReturnType: Option[Class[? <: Event]] = Some(classOf[Event])

    protected def filterStream: StampedEventFilter =
      identity

    implicit private val exceptionHandler: ExceptionHandler =
      ExceptionHandler:
        case t: ClosedException if t.getMessage != null =>
          ioRoute:
            for maybe <- isShuttingDown yield
              if maybe.isDefined then
                complete(ServiceUnavailable -> ShuttingDownProblem)
              else
                complete(ServiceUnavailable -> Problem.pure(t.getMessage))
        //case t: pekko.pattern.AskTimeoutException =>  // When getting EventWatch (Actor maybe terminated)
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

    private def jsonSeqEvents(eventWatch: EventWatch): Route =
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
    : Route =
      parameter("timeout" ? defaultJsonSeqChunkTimeout) { timeout =>
        //implicit val x: ToEntityMarshaller[EventId] = jsonSeqMarshaller
        completeWithCheckedStream(`application/x-ndjson`):
          eventWatch
            .streamEventIds(Some(timeout))
            .map(_.map(_
              .pipe(o => maybeHeartbeat.fold(o)(o.echoRepeated))
              .map((eventId: EventId) => ByteString(eventId.toString))))
      }

    private def eventRoute(
      request: EventRequest[Event],
      maybeHeartbeat: Option[FiniteDuration],
      eventWatch: EventWatch)
    : Route =
      extractRequest { httpRequest =>
        completeWithCheckedStream(`application/x-ndjson`):
          maybeHeartbeat match
            case None =>
              // Await the first event to check for Torn and convert it to a proper error message, otherwise continue with stream
              awaitFirstEventStream(request, eventWatch)

            case Some(heartbeat) =>
              IO.pure(Right(
                encodeAndHeartbeatStream(
                  heartbeatingStream(request, heartbeat, eventWatch),
                  shutdownSignaled, chunkSize = chunkSize
                ).interruptWhen(shutdownSignaled)))
      }

    private def awaitFirstEventStream(request: EventRequest[Event], eventWatch: EventWatch)
    : IO[Checked[Stream[IO, ByteString]]] =
      IO.defer:
        val runningSince = now
        val initialRequest = request.copy[Event](
          limit = 1 min request.limit)
        eventWatch.when(initialRequest, isRelevantEvent).map:
          case TearableEventSeq.Torn(eventId) =>
            Left(EventSeqTornProblem(requestedAfter = request.after, tornEventId = eventId))

          case EventSeq.Empty(_) =>
            Right(Stream.empty)

          case EventSeq.NonEmpty(closeableIterator) =>
            val head = autoClosing(closeableIterator)(_.next())

            val tailRequest = request.copy[Event](
              after = head.eventId,
              limit = request.limit - 1,
              // FIXME it should be max, not min:
              delay = (request.delay - runningSince.elapsed) min ZeroDuration)

            Right(encodeAndHeartbeatStream(
              head +: eventStream(tailRequest, isRelevantEvent, eventWatch),
              shutdownSignaled, chunkSize = chunkSize))

    //private def emptyResponseMarshallable(implicit s: JsonEntityStreamingSupport)
    //: ToResponseMarshallable =
    //  implicit val x: ToEntityMarshaller[Unit] = jsonSeqMarshaller
    //  streamToMarshallable(
    //    Stream.empty)

    private def heartbeatingStream(
      request: EventRequest[Event],
      heartbeat: FiniteDuration,
      eventWatch: EventWatch)
    : Stream[IO, Stamped[AnyKeyedEvent]] =
      // TODO Check if torn then return IO.raiseError
      logger.traceStream("### heartbeatingStream", s"heartbeat=$heartbeat"):
        StampedHeartbeat +:
          eventStream(request, isRelevantEvent, eventWatch)
            .keepAlive(heartbeat, StampedHeartbeatIO)

    private def eventStream(
      request: EventRequest[Event],
      predicate: AnyKeyedEvent => Boolean,
      eventWatch: EventWatch)
    : Stream[IO, Stamped[AnyKeyedEvent]] =
      filterStream(
        eventWatch  // Continue with an Stream, skipping the already read event
          .stream(request, predicate)
      ) .recoverWith:
        case NonFatal(e) =>
          logger.warn(e.toStringWithCauses)
          if e.getStackTrace.nonEmpty then logger.debug(e.toStringWithCauses, e)
          Stream.empty  // The streaming event web service doesn't have an error channel, so we simply end the tail

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

  //private def streamToMarshallable[A: Tag](stream: Stream[IO, A])
  //  (implicit q: Source[A, NotUsed] => ToResponseMarshallable)
  //: ToResponseMarshallable =
  //  monixStreamToMarshallable(
  //    stream.interruptWhen(shutdownSignaled))


object GenericEventRoute:
  type StampedEventFilter =
    Stream[IO, Stamped[KeyedEvent[Event]]] =>
      Stream[IO, Stamped[KeyedEvent[Event]]]

  private val logger = Logger[this.type]
