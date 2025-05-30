package js7.journal

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import com.softwaremill.tagging.@@
import fs2.Stream
import js7.base.catsutils.CatsEffectUtils.promiseIO
import js7.base.circeutils.typed.TypedJsonCodec.typeName
import js7.base.fs2utils.StreamExtensions.repeatLast
import js7.base.generic.Accepted
import js7.base.log.{CorrelId, Logger}
import js7.base.monixlike.FutureCancelable
import js7.base.problem.{Checked, ProblemException}
import js7.base.thread.Futures.promiseFuture
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.base.utils.Atomic
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.StackTraces.StackTraceThrowable
import js7.common.pekkoutils.ReceiveLoggingActor
import js7.data.event.{AnyKeyedEvent, Event, EventCalc, EventDrivenState, EventId, JournaledState, KeyedEvent, MaybeTimestampedKeyedEvent, Stamped, TimeCtx}
import js7.journal.JournalingActor.*
import js7.journal.configuration.JournalConf
import org.apache.pekko.actor.{Actor, ActorLogging, ActorRef, Stash}
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.Deadline.now
import scala.concurrent.{Future, Promise}
import scala.util.Success
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
trait JournalingActor[S <: JournaledState[S], E <: Event]
extends Actor, Stash, ActorLogging, ReceiveLoggingActor:

  protected def journalActor: ActorRef @@ JournalActor.type
  protected def journalConf: JournalConf
  protected def ioRuntime: IORuntime

  private given IORuntime = ioRuntime

  private var stashingCount = 0
  private val persistStatistics = new PersistStatistics
  private var _persistedEventId = EventId.BeforeFirst
  private val journalingTimer = Atomic(FutureCancelable.empty)

  become("receive")(receive)

  protected final def persistedEventId = _persistedEventId

  protected final def persistedEventId_=(eventId: EventId): Unit = _persistedEventId = eventId  // TODO Used for recovery, should not be mutable

  override protected def become(stateName: String)(receive: Receive): Unit =
    super.become(stateName)(journaling orElse receive)

  override def postStop(): Unit =
    journalingTimer.get().cancelAndForget()
    super.postStop()

  protected final def persistKeyedEventIO[A](
    eventCalc: EventCalc[S, E, TimeCtx], async: Boolean = false)
    (callback: Persisted[S, E] => A)
  : IO[Checked[A]] =
    promiseIO[Checked[A]]: promise =>
      self ! Persist(eventCalc, async = async, callback, promise)

  /** Fast lane for events not affecting the journaled state. */
  @TestOnly
  protected final def persistKeyedEventAcceptEarlyIO[EE <: E](
    eventCalc: EventCalc[S, EE, TimeCtx],
    options: CommitOptions = CommitOptions.default)
  : IO[Checked[Accepted]] =
    promiseIO[Checked[Accepted]]: promise =>
      self ! PersistAcceptEarly(eventCalc.widen, options, promise)

  protected final def persist[EE <: E, A](
    keyedEvent: MaybeTimestampedKeyedEvent[EE])(
    callback: (Stamped[KeyedEvent[EE]], S) => A)
  : Future[A] =
    persistKeyedEvent(keyedEvent)(callback)

  protected final def persistKeyedEvent[EE <: E, A](
    keyedEvent: MaybeTimestampedKeyedEvent[EE],
    async: Boolean = false)(
    callback: (Stamped[KeyedEvent[EE]], S) => A)
  : Future[A] =
    persist(EventCalc.pure(keyedEvent), async = async): persisted =>
      persisted.checkedSingle.orThrow
      callback(persisted.stampedKeyedEvents.head, persisted.aggregate)

  protected final def persist[EE <: E, A](
    eventCalc: EventCalc[S, EE, TimeCtx],
    options: CommitOptions = CommitOptions.default,
    async: Boolean = false)(
    callback: Persisted[S, EE] => A)
  : Future[A] =
    persistEventCalcReturnChecked2(eventCalc, options, async)(callback)
      .map(_.orThrow)(using ioRuntime.compute)

  protected final def persistKeyedEventsReturnChecked[EE <: E, A](
    eventCalc: EventCalc[S, EE, TimeCtx],
    options: CommitOptions = CommitOptions.default,
    async: Boolean = false)(
    callback: Persisted[S, EE] => A)
  : Future[Checked[A]] =
    persistEventCalcReturnChecked2(eventCalc, options, async, dontCrashActorOnFailure = true)(
      callback)

  private def persistEventCalcReturnChecked2[EE <: E, A](
    eventCalc: EventCalc[S, EE, TimeCtx],
    options: CommitOptions = CommitOptions.default,
    async: Boolean = false,
    dontCrashActorOnFailure: Boolean = false)(
    callback: Persisted[S, EE] => A)
  : Future[Checked[A]] =
    promiseFuture[Checked[A]] { promise =>
      start(async = async, "persistEventCalcReturnChecked2")
      journalActor.forward(
        JournalActor.Input.Store(CorrelId.current, eventCalc, self, options, since = now,
          callersItem = EventsCallback(
            CorrelId.current,
            async = async,
            callback = {
              case Left(problem) =>
                if !dontCrashActorOnFailure then
                  logger.error(s"»$toString« Event could not be stored: $problem")
                  throw problem.throwable.appendCurrentStackTrace
                logger.debug(s"»$toString« 💥 $problem$stashingCountRemaining")
                promise.complete(Success(Left(problem)))

              case Right(persisted) =>
                promise.complete(
                  try Success(Right(
                    callback(persisted.asInstanceOf[Persisted[S, EE]])))
                  catch { case NonFatal(t) =>
                    // TODO Ein Fehler sollte zum Abbruch führen? Aber dann?
                    logger.error(s"»$toString« ${t.toStringWithCauses}\n", t)
                    throw t
                    //Failure(t)
                  })
            })))
    }

  @TestOnly
  protected final def defer(callback: => Unit): Unit =
    defer_(async = false, callback)

  @TestOnly
  protected final def deferAsync(callback: => Unit): Unit =
    defer_(async = true, callback)

  @TestOnly
  private def defer_(async: Boolean, callback: => Unit): Unit =
    start(async = async, "defer")
    journalActor.forward(
      JournalActor.Input.Store(CorrelId.current, EventCalc.empty, self, CommitOptions.default, since = now,
        callersItem = Deferred(CorrelId.current, async = async,
          callback = {
            case Left(problem) => throw problem.throwable.appendCurrentStackTrace
            case Right(Accepted) => callback
          })))

  private def start(async: Boolean, firstName: => String): Unit =
    if stashingCount == Inhibited then throw new IllegalStateException("Journaling is inhibited")  // Avoid deadlock when waiting for response of dead JournalActor
    if !async then
      // async = false (default) lets Actor stash all messages but JournalActor.Output.Stored.
      // async = true means, message Store is intermixed with other messages.
      beginStashing(firstName)

  protected[journal] def journaling: Receive =
    case Persist(eventCalc: EventCalc[S, E, TimeCtx] @unchecked, async, callback, promise) =>
      promise.completeWith:
        persist(eventCalc, async = async): persisted =>
          try Right(callback(persisted))
          catch
            case ProblemException(problem) => Left(problem)
            case t: Throwable => throw t.appendCurrentStackTrace

    case PersistAcceptEarly(eventCalc, options, promise) =>
      start(async = true, "persistKeyedEventAcceptEarlyIO")
      journalActor.forward(
        JournalActor.Input.Store(CorrelId.current, eventCalc, self, options, since = now, commitLater = true,
          Deferred(CorrelId.current, async = true,
            callback = checked => promise.success(checked))))

    case JournalActor.Output.Stored(persisted, item: Item) =>
      // sender() is from persistKeyedEvent or deferAsync
      item.correlId.bind[Unit]:
        if !item.async then
          endStashing(persisted.map(_.stampedKeyedEvents) getOrElse Nil)

        persisted match
          case Left(problem) =>
            item match
              case eventsCallback: EventsCallback =>
                eventsCallback.callback(Left(problem))

              case deferred: Deferred =>
                deferred.callback(Left(problem))

              case _ => sys.error(
                s"JournalActor.Output.Stored($problem) · Message does not match item '$item'")

          case Right(persisted) =>
            persistStatistics.onStored(persisted.stampedKeyedEvents.size)
            persisted.stampedKeyedEvents.lastOption foreach { last =>
              _persistedEventId = last.eventId
            }
            (persisted, item) match
              case (_, eventsCallback: EventsCallback) =>
                if TraceLog && logger.underlying.isTraceEnabled then for st <- persisted.stampedKeyedEvents do
                  logger.trace(s"»$toString« Stored ${EventId.toString(st.eventId)} ${st.value.key} <-: ${typeName(st.value.event.getClass)}$stashingCountRemaining")
                eventsCallback.callback(Right(persisted.asInstanceOf[Persisted[S, E]]))

              case (Persisted(_, Nil, _), deferred: Deferred) =>
                if TraceLog then logger.trace(s"»$toString« Stored (no event)$stashingCountRemaining")
                deferred.callback(Right(Accepted))

              case _ => sys.error(
                s"JournalActor.Output.Stored(${persisted.stampedKeyedEvents.length}×) message does not match item '$item'")

    case JournalActor.Output.Accepted(item: Item) =>
      // sender() is from persistKeyedEvent or deferAsync
      if !item.async then
        endStashing(Nil)
      item match
        case Deferred(correlId, _, callback) =>
          correlId.bind:
            if TraceLog then logger.trace(s"»$toString« Stored (events are written, not flushed)$stashingCountRemaining")
            callback(Right(Accepted))

        case _ => sys.error(s"JournalActor.Output.Accepted message does not match item '$item'")

    case msg if stashingCount > 0 =>
      if TraceLog then logger.trace(s"»$toString« 🟠 Still waiting for event commit: stash $msg")
      super.stash()

  private def beginStashing(firstName: => String): Unit =
    stashingCount += 1
    if stashingCount == 1 then
      persistStatistics.beginStashing(firstName)
      logBecome("journaling")
      context.become(journaling, discardOld = false)
      if journalConf.persistWarnDurations.nonEmpty then
        logger.whenWarnEnabled:
          val since = now
          val timer = FutureCancelable:
            Stream.iterable(journalConf.persistWarnDurations)
              .covary[IO]
              .repeatLast
              .evalMap(IO.sleep)
              .foreach(_ => IO:
                // Under load it may be normal to be busy for some time ???
                logger.warn(s"»$toString« 🟠 Still persisting for ${
                  since.elapsed.pretty} ($stashingCount persist operations in progress)"))
              .compile.drain
              .unsafeRunCancelable()
          journalingTimer.getAndSet(timer).cancelAndForget()

  private def endStashing(stamped: Seq[Stamped[AnyKeyedEvent]]): Unit =
    if stashingCount == 0 then
      val msg = s"Journal Stored message received (duplicate? stash in callback?) but stashingCount=$stashingCount: $stamped"
      logger.error(s"»$toString« $msg")
      throw new RuntimeException(msg)
    stashingCount -= 1
    if stashingCount == 0 then
      journalingTimer.get().cancelAndForget()
      unstashAll()
      persistStatistics.endStashing()
      if TraceLog then logger.trace(s"»$toString« unbecome")
      context.unbecome()

  private def stashingCountRemaining = (stashingCount > 0) ?? s", $stashingCount remaining"

  private case class EventsCallback(
    correlId: CorrelId,
    async: Boolean,
    callback: Checked[Persisted[S, E]] => Unit)
  extends Item:
    override def toString = s"EventsCallback(${async ?? "async"})"

  private case class Deferred(
    correlId: CorrelId,
    async: Boolean,
    callback: Checked[Accepted] => Unit)
  extends Item:
    override def toString = s"Deferred(${async ?? "async"})"

  private case class Persist[A](
    eventCalc: EventCalc[S, E, TimeCtx],
    async: Boolean = false,
    callback: Persisted[S, E] => A,
    promise: Promise[Checked[A]])

  private case class PersistAcceptEarly(
    eventCalc: EventCalc[S, E, TimeCtx],
    options: CommitOptions,
    promise: Promise[Checked[Accepted]])

  private class PersistStatistics:
    private var persistStartedAt = now/*dummy*/
    private var persistCount = 0
    private var eventCount = 0
    private var firstName = ""

    def beginStashing(firstName: String): Unit =
      this.firstName = firstName
      persistStartedAt = now
      eventCount = 0
      persistCount = 0

    def onStored(eventCount: Int): Unit =
      persistCount += 1
      this.eventCount += eventCount

    def endStashing(): Unit =
      val duration = persistStartedAt.elapsed
      if duration >= BigStoreThreshold then
        logger.debug(s"»${JournalingActor.this.toString}« Long persist completed ($persistCount×, $firstName ...) - " +
          itemsPerSecondString(duration, eventCount, "events"))


object JournalingActor:
  private val Inhibited = -1
  private val logger = Logger[this.type]
  private val TraceLog = false
  private val BigStoreThreshold = 1.s

  final case class Timestamped[+E <: Event](keyedEvent: KeyedEvent[E], timestampMillis: Option[Long] = None)
  extends JournalActor.Timestamped

  private sealed trait Item extends JournalActor.CallersItem:
    def async: Boolean
    def correlId: CorrelId
