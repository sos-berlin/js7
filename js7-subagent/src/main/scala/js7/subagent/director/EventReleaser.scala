package js7.subagent.director

import cats.effect.{FiberIO, IO, Ref}
import cats.syntax.foldable.*
import fs2.concurrent.SignallingRef
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.event.{AnyKeyedEvent, EventId, KeyedEvent, Stamped}
import js7.data.order.{OrderEvent, OrderId}
import js7.data.subagent.{SubagentId, SubagentRunId}
import js7.subagent.director.EventReleaser.*
import scala.concurrent.duration.FiniteDuration

/** Sends ReleaseEvents to the Subagent, outside the CommandDispatcher queue.
  *
  * The CommandDispatcher queue may be blocked by a command which waits for the Subagent's
  * MemoryJournal, which in turn waits for ReleaseEvents.
  *
  * Only the highest requested EventId is sent.
  */
private final class EventReleaser private(
  subagentId: SubagentId,
  releaseEventsDelay: FiniteDuration,
  state: SignallingRef[IO, State],
  awaiting: SignallingRef[IO, EventId],
  fiber: Ref[IO, Option[FiberIO[Unit]]]):

  private val logger = Logger.withPrefix[this.type](subagentId.toString)

  /** Start sending ReleaseEvents for `subagentRunId`.
    *
    * When recoupling with the same SubagentRunId, a not yet released EventId is sent again.
    */
  def start(subagentRunId: SubagentRunId)(postReleaseEvents: EventId => IO[Checked[Unit]])
  : IO[Unit] =
    cancelFiber
      .productR:
        state.update: s =>
          if s.subagentRunId.contains(subagentRunId) then s else State(Some(subagentRunId))
      .productR:
        run(subagentRunId, postReleaseEvents).start
      .flatMap: started =>
        fiber.set(Some(started))

  /** Stop and let `awaitReleased` return, because the Subagent's run has ended. */
  def stop: IO[Unit] =
    state.update(_.copy(subagentRunId = None)) *>
      cancelFiber

  private def cancelFiber: IO[Unit] =
    fiber.getAndSet(None).flatMap(_.foldMap(_.cancel))

  /** Register the EventIds to be released.
    *
    * @param stampedKeyedEvents The events to be released (only EventIds and OrderIds are of
    *                           interest)
    */
  def releaseInBackground(
    stampedKeyedEvents: Vector[Stamped[AnyKeyedEvent]],
    lastEventId: EventId)
  : IO[Unit] =
    releaseInBackground(lastEventId, None) *>
      stampedKeyedEvents.view.flatMap: stamped =>
        stamped.value match
          case KeyedEvent(orderId: OrderId, _: OrderEvent) => Some(orderId -> stamped.eventId)
          case _ => None
      .toMap // Last EventId per OrderId wins
      .toVector.foldMapM: (orderId, eventId) =>
        releaseInBackground(eventId, Some(orderId))

  /** Request ReleaseEvents(eventId), and register `eventId` as the last EventId of `orderId`.
    * Does not wait. */
  private def releaseInBackground(eventId: EventId, orderId: Option[OrderId] = None): IO[Unit] =
    orderId match
      case None =>
        state.update(s => s.copy(requested = s.requested max eventId))
      case Some(orderId) =>
        state.update: s =>
          s.copy(
            requested = s.requested max eventId,
            orderToEventId = s.orderToEventId.updated(
              orderId,
              s.orderToEventId.get(orderId).fold(eventId)(_ max eventId)))

  /** Request ReleaseEvents(eventId), not related to a particular Order. Does not wait. */
  private def releaseInBackground(eventId: EventId): IO[Unit] =
    state.update(s => s.copy(requested = s.requested max eventId))

  /** Wait until the last EventId requested for `orderId` has been released by the Subagent,
    * or until the Subagent's run has ended. */
  def awaitReleased(orderId: OrderId): IO[Unit] =
    state.get.flatMap: s0 =>
      s0.orderToEventId.get(orderId).fold(IO.unit): eventId =>
        IO.unlessA(s0.subagentRunId.isEmpty || s0.released >= eventId):
          IO.whenA(releaseEventsDelay.isPositive):
            awaiting.update(_ max eventId)
          *>
            state.waitUntil: s =>
              s.subagentRunId != s0.subagentRunId || s.released >= eventId

  private def run(subagentRunId: SubagentRunId, postReleaseEvents: EventId => IO[Checked[Unit]])
  : IO[Unit] =
    state.discrete
      .takeWhile(_.subagentRunId.contains(subagentRunId))
      .filter(s => s.requested > s.released)
      .map(_.requested)
      .pipeIf(releaseEventsDelay.isPositive):
        _.evalTap: eventId =>
          awaiting.waitUntil(_ >= eventId).timeoutTo(releaseEventsDelay, IO.unit)
      .evalMap: eventId =>
        // 👉 Higher throughput could be achieved by continuously streaming the released events.
        // Only needed for (very) big stdout outputs or many very short (internal) processes.
        postReleaseEvents(eventId).flatMap:
          case Left(problem) =>
            // If the Subagent answers ReleaseEvents with a problem, it's logged and the next
            // release covers that EventId. Nothing retries it if no new events arrive.
            // A later ReleaseEvents will release this EventId, too.
            IO(logger.warn(s"ReleaseEvents($eventId) => $problem"))
          case Right(()) =>
            state.update: s =>
              if s.subagentRunId.contains(subagentRunId) then
                val released = s.released max eventId
                s.copy(
                  released = released,
                  orderToEventId = s.orderToEventId.filter(_._2 > released))
              else
                s
      .compile.drain
      .handleErrorWith: t =>
        IO(logger.error(s"EventReleaser($subagentRunId) => ${t.toStringWithCauses}", t))

  override def toString = s"EventReleaser($subagentId)"


private object EventReleaser:

  def apply(subagentId: SubagentId, releaseEventsDelay: FiniteDuration): IO[EventReleaser] =
    for
      state <- SignallingRef[IO].of(State())
      awaiting <- SignallingRef[IO].of(EventId.BeforeFirst)
      fiber <- Ref[IO].of(Option.empty[FiberIO[Unit]])
    yield
      new EventReleaser(subagentId, releaseEventsDelay, state, awaiting, fiber)

  private final case class State(
    subagentRunId: Option[SubagentRunId] = None,
    requested: EventId = EventId.BeforeFirst,
    released: EventId = EventId.BeforeFirst,
    /** The last requested EventId per OrderId, until released. */
    orderToEventId: Map[OrderId, EventId] = Map.empty)
