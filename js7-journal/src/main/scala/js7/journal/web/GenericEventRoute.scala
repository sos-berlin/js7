package js7.journal.web

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import fs2.Stream
import izumi.reflect.Tag
import js7.base.auth.ValidUserPermission
import js7.base.catsutils.Environment
import js7.base.circeutils.CirceUtils.RichJsonObject
import js7.base.fs2utils.StreamExtensions.*
import js7.base.log.Logger
import js7.base.problem.Problems.ShuttingDownProblem
import js7.base.problem.{Checked, Problem, ProblemException}
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.http.JsonStreamingSupport.*
import js7.common.http.PekkoHttpClient.HttpHeartbeatByteString
import js7.common.pekkohttp.PekkoHttpServerUtils.{accept, completeWithCheckedStream, encodeParallel}
import js7.common.pekkohttp.StandardDirectives
import js7.common.pekkohttp.StandardDirectives.ioRoute
import js7.common.pekkohttp.StandardMarshallers.*
import js7.common.pekkohttp.web.session.RouteProvider
import js7.common.pekkoutils.ByteStrings.syntax.ByteStringToByteSequence
import js7.data.Problems.AckFromActiveClusterNodeProblem
import js7.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, EventSeq, EventSeqTornProblem, KeyedEvent, KeyedEventTypedJsonCodec, Stamped, TearableEventSeq}
import js7.data.system.ServerMeteringEvent
import js7.journal.watch.{ClosedException, EventWatch}
import js7.journal.web.EventDirectives.eventRequest
import js7.journal.web.GenericEventRoute.*
import org.apache.pekko.actor.ActorRefFactory
import org.apache.pekko.http.scaladsl.model.StatusCodes.ServiceUnavailable
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.{Directive, Directive1, ExceptionHandler, Route}
import org.apache.pekko.util.ByteString
import scala.concurrent.duration.*
import scala.concurrent.duration.Deadline.now
import scala.util.chaining.*

/**
  * @author Joacim Zschimmer
  */
trait GenericEventRoute extends RouteProvider:
  protected implicit def actorRefFactory: ActorRefFactory
  protected def eventWatch: EventWatch

  private given IORuntime = ioRuntime

  private lazy val defaultJsonSeqChunkTimeout =
    config.getDuration("js7.web.server.services.event.streaming.chunk-timeout").toFiniteDuration
  private lazy val minimumStreamingDelay =
    config.getDuration("js7.web.server.services.event.streaming.delay").toFiniteDuration

  final def genericEventRoute(jsonCodec: KeyedEventTypedJsonCodec[Event]): Route =
    new GenericEventRouteProvider:
      protected def keyedEventTypedJsonCodec = jsonCodec
    .route

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
              authorizedUser(ValidUserPermission): user =>
                val waitingSince = !eventWatch.whenStarted.isCompleted ? now
                if waitingSince.isDefined then logger.debug("Waiting for journal to become ready ...")
                onSuccess(eventWatch.whenStarted): eventWatch =>
                  for o <- waitingSince do logger.debug("Journal has become ready after " +
                    o.elapsed.pretty + ", continuing event web service")
                  Route.seal:
                    jsonSeqEvents(eventWatch)

    private def jsonSeqEvents(eventWatch: EventWatch): Route =
      parameter("onlyAcks" ? false): onlyAcks =>
        parameter("heartbeat".as[FiniteDuration].?): heartbeat =>
          if onlyAcks then
            eventIdRoute(eventWatch, heartbeat)
          else
            parameter("serverMetering".as[FiniteDuration].?): serverMetering =>
              eventDirective(eventWatch.lastAddedEventId): request =>
                eventRoute(eventWatch, request, heartbeat, serverMetering)

    private def eventIdRoute(eventWatch: EventWatch, maybeHeartbeat: Option[FiniteDuration])
    : Route =
      parameter("timeout" ? defaultJsonSeqChunkTimeout): timeout =>
        completeWithCheckedStream(`application/x-ndjson`):
          eventWatch
            .streamEventIds(Some(timeout))
            .map(_.map(_
              .map((eventId: EventId) => ByteString(eventId.toString) ++ LF)
              .recover:
                case ProblemException(problem @ AckFromActiveClusterNodeProblem) =>
                  Problem.typedJsonEncoder.encodeObject(problem).toByteArray.toByteString
              .pipe: stream =>
                maybeHeartbeat.fold(stream): h =>
                  stream.keepAlive(h, IO.pure(HttpHeartbeatByteString))))

    private def eventRoute(
      eventWatch: EventWatch,
      request: EventRequest[Event],
      maybeHeartbeat: Option[FiniteDuration],
      maybeServerMetering: Option[FiniteDuration])
    : Route =
      completeWithCheckedStream(`application/x-ndjson`):
        if maybeHeartbeat.isEmpty && maybeServerMetering.isEmpty then
          // Await the first event to check for Torn and convert it to a proper error message,
          // otherwise continue with stream
          awaitFirstEventStream(request, eventWatch)
        else
          IO:
            eventWatch.checkEventId(request.after) >> Right:
              eventStream(request, isRelevantEvent, eventWatch, maybeServerMetering)
                .through:
                  encodeParallel(httpChunkSize = httpChunkSize, prefetch = prefetch)
                // Heartbeat only if it's faster then serverMetering
                .pipeMaybe(maybeHeartbeat.filter(h => maybeServerMetering.forall(h < _))):
                  (stream, h) =>
                    stream.keepAlive(h, IO.pure(HttpHeartbeatByteString))
                .prependOne(HttpHeartbeatByteString)
                .interruptWhenF(shutdownSignaled)

    private def awaitFirstEventStream(request: EventRequest[Event], eventWatch: EventWatch)
    : IO[Checked[Stream[IO, ByteString]]] =
      IO.defer:
        val runningSince = now
        val initialRequest = request.copy[Event](
          limit = 1 min request.limit)
        // While waiting for the first event, no ServerMeteringEvent is emitted !!!
        // Not an issue, because client combines serverMetering with heartbeat.
        eventWatch.when(initialRequest, isRelevantEvent).map:
          case TearableEventSeq.Torn(eventId) =>
            Left(EventSeqTornProblem(requestedAfter = request.after, tornEventId = eventId))

          case EventSeq.Empty(_) =>
            Right(Stream.empty)

          case EventSeq.NonEmpty(closeableIterator) =>
            val head = autoClosing(closeableIterator)(_.next())

            val tailRequest = request.copy[Event](
              after = head.eventId,
              limit = request.limit - 1 max 0,
              delay = (request.delay - runningSince.elapsed) max ZeroDuration)

            eventWatch.checkEventId(request.after) >> Right:
              Stream.emit(head)
                .append:
                  eventStream(tailRequest, isRelevantEvent, eventWatch)
                .through:
                  encodeParallel(httpChunkSize = httpChunkSize, prefetch = prefetch)
                .interruptWhenF(shutdownSignaled)

    private def eventStream(
      request: EventRequest[Event],
      predicate: AnyKeyedEvent => Boolean,
      eventWatch: EventWatch,
      serverMetering: Option[FiniteDuration] = None)
    : Stream[IO, Stamped[AnyKeyedEvent]] =
      filterStream:
        eventWatch.stream(request, predicate)
          .pipeMaybe(serverMetering): (stream, serverMetering) =>
            stream.mergeHaltL:
              priorityDataStream(every = serverMetering)
          .handleErrorWith: t =>
            logger.warn(t.toStringWithCauses)
            if t.getStackTrace.nonEmpty then logger.debug(t.toStringWithCauses, t)
            // The streaming event web service doesn't have an error channel,
            // so we simply end the tail
            Stream.empty

    private def priorityDataStream(every: FiniteDuration)
    : Stream[IO, Stamped[KeyedEvent[ServerMeteringEvent]]] =
      Stream.eval:
        Environment.maybe[TestWiring]
      .flatMap: maybeTestWiring =>
        Stream.fixedDelay[IO](every).map: _ =>
          var event = ServerMeteringEvent.fromCurrentMxBean()
          for w <- maybeTestWiring do
            event = event.copy(testMeteringValue = w.testMeteringValue)
          // EventId.Missing, because ServerMeteringEvent is a NonPersistentEvent
          Stamped(EventId.Missing, Timestamp.now, KeyedEvent(event))

    private def eventDirective(defaultAfter: EventId)
    : Directive1[EventRequest[Event]] =
      Directive(inner =>
        eventRequest[Event](
          defaultAfter = Some(defaultAfter),
          minimumDelay = minimumStreamingDelay,
          defaultTimeout = defaultJsonSeqChunkTimeout,
          defaultReturnType = defaultReturnType.map(_.simpleScalaName))
        .apply(eventRequest => inner(Tuple1(eventRequest))))


object GenericEventRoute:

  type StampedEventFilter =
    Stream[IO, Stamped[KeyedEvent[Event]]] =>
      Stream[IO, Stamped[KeyedEvent[Event]]]

  private val logger = Logger[this.type]
  private val LF = ByteString("\n")

  final case class TestWiring(testMeteringValue: Option[Double])
