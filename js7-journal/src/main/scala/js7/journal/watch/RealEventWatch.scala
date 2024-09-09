package js7.journal.watch

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import fs2.Stream
import izumi.reflect.Tag
import java.util.concurrent.TimeoutException
import js7.base.ProvisionalAssumptions
import js7.base.catsutils.SyncDeadline
import js7.base.fs2utils.StreamExtensions.+:
import js7.base.fs2utils.StreamUtils
import js7.base.fs2utils.StreamUtils.closeableIteratorToStream
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.metering.CallMeter
import js7.base.problem.Checked
import js7.base.stream.IncreasingNumberSync
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.base.utils.CatsUtils.syntax.*
import js7.base.utils.CloseableIterator
import js7.base.utils.ScalaUtils.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.Problems.AckFromActiveClusterNodeProblem
import js7.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, EventSeq, KeyedEvent, Stamped, TearableEventSeq}
import js7.data.problems.UnknownEventIdProblem
import js7.journal.watch.RealEventWatch.*
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.*
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
trait RealEventWatch extends EventWatch:

  protected def eventsAfter(after: EventId): Option[CloseableIterator[Stamped[KeyedEvent[Event]]]]

  protected def isActiveNode: Boolean

  // Lazy, initialize only after whenStarted has been called!
  private lazy val committedEventIdSync =
    new IncreasingNumberSync(initial = tornEventId, o => "EventId " + EventId.toString(o))

  private[journal] final def onEventsCommitted(eventId: EventId): Unit =
    committedEventIdSync.onAdded(eventId)

  final def stream[E <: Event](
    request: EventRequest[E],
    predicate: KeyedEvent[E] => Boolean,
    onlyAcks: Boolean)
  : Stream[IO, Stamped[KeyedEvent[E]]] =
    val originalTimeout = request.timeout

    Stream.unfoldEval(IO.pure(request)): getRequest =>
      // Access the in previous iteration computed values lastEventId and limit (see below)
      // Timeout is renewed after every fetched event
      getRequest.flatMap: request =>
        if request.limit <= 0 then
          IO.none
        else
          toDeadline(request.timeout).flatMap: deadline =>
            when[E](request, predicate).flatMap:
              case TearableEventSeq.Torn(tornAfter) =>
                IO.raiseError:
                  if onlyAcks && isActiveNode then
                    AckFromActiveClusterNodeProblem.throwable
                  else
                    // TODO Use appropriate exception:
                    TornException(after = request.after, tornEventId = tornAfter)

              case EventSeq.Empty(lastEventId) =>
                deadline.fold(IO.none): deadline =>
                  SyncDeadline.usingNow(Some(deadline.timeLeft))
                .map: timeLeft =>
                  timeLeft.forall(_.isPositive).thenSome:
                    Stream.empty -> IO: /*This will be getRequest for the next iteration:*/
                      request.copy[E](
                        after = lastEventId,
                        timeout = timeLeft)

              case EventSeq.NonEmpty(events) =>
                IO:
                  if events.isEmpty then
                    throw IllegalStateException("EventSeq.NonEmpty(EMPTY)") // Avoid loop
                  val iterator = if onlyAcks then lastOfIterator(events) else events
                  var lastEventId = request.after
                  var limit = request.limit
                  val stream = closeableIteratorToStream(
                    iterator,
                    chunkSize = ProvisionalAssumptions.streamChunks.elementsPerChunkLimit
                  ).map: o =>
                    lastEventId = o.eventId
                    limit -= 1
                    o
                  // Closed-over lastEventId and limit are updated as stream is consumed,
                  // therefore defer access to final values (see above)
                  Some((stream, IO(request.copy[E](
                    after = lastEventId, limit = limit, timeout = originalTimeout))))
    .flatten

  final def streamEventIds(maybeTimeout: Option[FiniteDuration] = None)
  : IO[Checked[Stream[IO, EventId]]] =
    toDeadline(maybeTimeout).flatMap: deadline =>
      IO:
        if isActiveNode then
          logger.debug(s"🚫 $AckFromActiveClusterNodeProblem")
          Left(AckFromActiveClusterNodeProblem)
        else
          Right(streamEventIds2(deadline))

  private def streamEventIds2(deadline: Option[SyncDeadline]): Stream[IO, EventId] =
    mustNotBeActive:
      val lastEventId = lastAddedEventId
      lastEventId +:
        Stream.unfoldEval[IO, EventId, EventId](lastEventId): after =>
          committedEventIdSync.whenAvailable(after, until = deadline.map(_.toCatsDeadline))
            .map: available =>
              if available then
                Some:
                  val lastEventId = lastAddedEventId
                  lastEventId -> lastEventId
              else
                logger.debug:
                  "❓committedEventIdSync.whenAvailable returned false (elapsed but no timeout?)"
                None

  private def mustNotBeActive(stream: Stream[IO, EventId]): Stream[IO, EventId] =
    stream
      .evalTap(_ => IO.whenA(isActiveNode):
        logger.debug(s"🚫 $AckFromActiveClusterNodeProblem")
        IO.raiseError(AckFromActiveClusterNodeProblem.throwable))

  final def when[E <: Event](request: EventRequest[E], predicate: KeyedEvent[E] => Boolean)
  : IO[TearableEventSeq[CloseableIterator, KeyedEvent[E]]] =
    whenAny[E](request, request.eventClasses, predicate)

  final def whenAny[E <: Event](
    request: EventRequest[E],
    eventClasses: Set[Class[? <: E]],
    predicate: KeyedEvent[E] => Boolean)
  : IO[TearableEventSeq[CloseableIterator, KeyedEvent[E]]] =
    whenAnyKeyedEvents(
      request,
      collect = {
        case e if eventClasses.exists(_ isAssignableFrom e.event.getClass)
          && predicate(e.asInstanceOf[KeyedEvent[E]]) =>
          e.asInstanceOf[KeyedEvent[E]]
      })

  final def whenKeyedEvent[E <: Event](using E: Event.KeyCompanion[? >: E])(
    request: EventRequest[E],
    key: E.Key,
    predicate: E => Boolean)
  : IO[E] =
    whenKey[E](request.copy[E](limit = 1), key, predicate) map:
      case eventSeq: EventSeq.NonEmpty[CloseableIterator, E] @unchecked =>
        try eventSeq.stamped.next().value
        finally eventSeq.close()
      case _: EventSeq.Empty => throw new TimeoutException(s"Timed out: $request")
      case TearableEventSeq.Torn(tornEventId) => throw new TornException(request.after, tornEventId)

  final def whenKey[E <: Event](using E: Event.KeyCompanion[? >: E])
    (request: EventRequest[E], key: E.Key, predicate: E => Boolean)
  : IO[TearableEventSeq[CloseableIterator, E]] =
    whenAnyKeyedEvents(
      request,
      collect = {
        case e if request.matchesClass(e.event.getClass)
          && e.key == key && predicate(e.event.asInstanceOf[E]) =>
          e.event.asInstanceOf[E]
      })

  private def whenAnyKeyedEvents[E <: Event, A](
    request: EventRequest[E],
    collect: PartialFunction[AnyKeyedEvent, A])
  : IO[TearableEventSeq[CloseableIterator, A]] =
    toDeadline(request.timeout).flatMap: deadline =>
      whenAnyKeyedEvents2(request.after, deadline, request.delay, collect, request.limit,
        maybeTornOlder = request.tornOlder)

  private def whenAnyKeyedEvents2[A](
    after: EventId,
    deadline: Option[SyncDeadline],
    delay: FiniteDuration,
    collect: PartialFunction[AnyKeyedEvent, A],
    limit: Int,
    maybeTornOlder: Option[FiniteDuration])
  : IO[TearableEventSeq[CloseableIterator, A]] =
    val overheatingDurations = Iterator(10, 30, 70, 300, 600)
      .concat(Iterator.continually(1000))
      .map(_.ms)

    def untilNonEmpty(after: EventId): IO[TearableEventSeq[CloseableIterator, A]] =
      started
        .flatMap: _ =>
          committedEventIdSync.whenAvailable(after, deadline.map(_.toCatsDeadline), delay)
        .flatMap(eventArrived =>
          collectEventsSince(after, collect, limit) match {
            case eventSeq @ EventSeq.NonEmpty(iterator) =>
              maybeTornOlder match {
                case None => IO.pure(eventSeq)
                case Some(tornOlder) =>
                  // If the first event is not fresh, we have a read congestion.
                  // We serve a (simulated) Torn, and the client can fetch the current state
                  // and read fresh events, skipping the congestion.
                  IO.defer {
                    val head = iterator.next()
                    // Don't compare head.timestamp, timestamp may be much older)
                    if EventId.toTimestamp(head.eventId) + tornOlder < Timestamp.now then
                      iterator.close()
                      // Simulate a torn EventSeq
                      IO.pure(TearableEventSeq.Torn(committedEventIdSync.last))
                    else
                      IO.pure(EventSeq.NonEmpty(head +: iterator))
                  }
              }

            case empty @ EventSeq.Empty(lastEventId) =>
              SyncDeadline
                .usingNow: now ?=>
                  deadline.forall(_.hasTimeLeft) ? (
                    if lastEventId < committedEventIdSync.last then
                      // May happen due to race condition ???
                      val delay = overheatingDurations.next()
                      logger.debug(s"committedEventIdSync.whenAvailable(after=$after," +
                        s" timeout=${deadline.fold("")(_.timeLeft.pretty)})" +
                        s" last=${committedEventIdSync.last} => $eventArrived," +
                        s" unexpected $empty, delaying ${delay.pretty}")
                      delay
                    else
                      ZeroDuration)
                .flatMap:
                  case Some(preventOverheating) =>
                    untilNonEmpty(lastEventId).delayBy(preventOverheating) // Brake
                  case None =>
                    IO.pure(EventSeq.Empty(lastEventId)) // Repeat without delay

          case o: TearableEventSeq.Torn =>
            IO.pure(o)
        })

    untilNonEmpty(after)

  private def collectEventsSince[A](
    after: EventId,
    collect: PartialFunction[AnyKeyedEvent, A],
    limit: Int)
  : TearableEventSeq[CloseableIterator, A] =
    meterCollectEventsSince:
      val last = lastAddedEventId
      if after > last then
        logger.debug(s"The future event requested is not yet available, " +
          s"lastAddedEventId=${EventId.toString(last)} after=${EventId.toString(after)}")
        EventSeq.Empty(last)  // Future event requested is not yet available
      else
        eventsAfter(after) match
          case Some(stampeds) =>
            var lastEventId = after
            val eventIterator = stampeds
              .tapEach { o => lastEventId = o.eventId }
              .collect { case stamped if collect isDefinedAt stamped.value => stamped map collect }
              .take(limit)
            if eventIterator.isEmpty then
              eventIterator.close()
              EventSeq.Empty(lastEventId)
            else
              EventSeq.NonEmpty(eventIterator)
          case None =>
            TearableEventSeq.Torn(after = tornEventId)

  final def lastAddedEventId: EventId =
    committedEventIdSync.last

  final def checkEventId(eventId: EventId): Checked[Unit] =
    eventsAfter(eventId) match
      case Some(iterator) =>
        iterator.close()
        Checked.unit
      case None =>
        val problem = UnknownEventIdProblem(
          requestedEventId = eventId,
          tornEventId = tornEventId,
          lastAddedEventId = lastAddedEventId)
        logger.warn(s"$problem (tornEventId=$tornEventId lastAddedEventId=$lastAddedEventId)")
        Left(problem)

  /** TEST ONLY - Blocking. */
  @TestOnly
  final def await[E <: Event: ClassTag: Tag](
    predicate: KeyedEvent[E] => Boolean,
    after: EventId,
    timeout: FiniteDuration)
    (using IORuntime, sourcecode.Enclosing, sourcecode.FileName, sourcecode.Line)
  : Vector[Stamped[KeyedEvent[E]]] =
    awaitAsync[E](predicate, after, timeout)
      .await(timeout + 1.s)

  @TestOnly
  final def awaitAsync[E <: Event: ClassTag](
    predicate: KeyedEvent[E] => Boolean,
    after: EventId,
    timeout: FiniteDuration)
    (using IORuntime, sourcecode.Enclosing, sourcecode.FileName, sourcecode.Line)
  : IO[Vector[Stamped[KeyedEvent[E]]]] =
    lazy val label = s"awaitAsync[${implicitly[ClassTag[E]].runtimeClass.shortClassName}]" +
      s" in ${summon[sourcecode.FileName].value}:${summon[sourcecode.Line].value}"
    logger.debugIO(label)(
      when[E](EventRequest.singleClass[E](after = after, Some(timeout)), predicate)
        .map {
          case EventSeq.NonEmpty(events) =>
            try events.toVector
            finally events.close()

          case _: EventSeq.Empty =>
            throw new TimeoutException(s"RealEventWatch.await[${implicitClass[E].scalaName}]" +
              s"(after=$after, timeout=$timeout) timed out")

          //? case TearableEventSeq.Torn(tornEventId) =>
          //?   throw new TornException(after, tornEventId)

          case o =>
            sys.error(s"$label(after=$after,timeout=${timeout.pretty}) unexpected EventSeq: $o")
        }
        .logWhenItTakesLonger(label))


object RealEventWatch:
  private val logger = Logger[this.type]
  private val meterCollectEventsSince = CallMeter()

  private def toDeadline(timeout: Option[FiniteDuration]): IO[Option[SyncDeadline]] =
    timeout match
      // Protected against Timestamp overflow
      case Some(timeout) if timeout < EventRequest.LongTimeout =>
        SyncDeadline.usingNow: now ?=>
          Some(now + timeout)
      case _ => IO.none

  private def lastOfIterator[A <: AnyRef](iterator: CloseableIterator[A]): CloseableIterator[A] =
    var last = null.asInstanceOf[A]
    while iterator.hasNext do last = iterator.next()
    iterator.close()
    CloseableIterator.fromIterator(Option(last).iterator)
