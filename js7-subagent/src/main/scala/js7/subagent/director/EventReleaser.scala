package js7.subagent.director

import cats.effect.{FiberIO, IO, Ref}
import fs2.concurrent.SignallingRef
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.event.EventId
import js7.data.subagent.{SubagentId, SubagentRunId}
import js7.subagent.director.EventReleaser.*

/** Sends ReleaseEvents to the Subagent, outside the CommandDispatcher queue.
  *
  * The CommandDispatcher queue may be blocked by a command which waits for the Subagent's
  * MemoryJournal, which in turn waits for ReleaseEvents.
  *
  * Only the highest requested EventId is sent.
  */
private final class EventReleaser private(
  subagentId: SubagentId,
  state: SignallingRef[IO, State],
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

  /** Request ReleaseEvents(eventId). Does not wait. */
  def releaseInBackground(eventId: EventId): IO[Unit] =
    state.update(s => s.copy(requested = s.requested max eventId))

  /** Wait until all EventIds requested so far have been released by the Subagent,
    * or until the Subagent's run has ended. */
  def awaitReleased: IO[Unit] =
    state.get.flatMap: s0 =>
      IO.unlessA(s0.subagentRunId.isEmpty || s0.released >= s0.requested):
        state.waitUntil(s => s.subagentRunId != s0.subagentRunId || s.released >= s0.requested)

  private def run(subagentRunId: SubagentRunId, postReleaseEvents: EventId => IO[Checked[Unit]])
  : IO[Unit] =
    state.discrete
      .takeWhile(_.subagentRunId.contains(subagentRunId))
      .filter(s => s.requested > s.released)
      .map(_.requested)
      .changes // discrete skips intermediate values, so only the latest EventId is sent
      .evalMap: eventId =>
        postReleaseEvents(eventId).flatMap:
          case Left(problem) =>
            // If the Subagent answers ReleaseEvents with a problem, it's logged and the next
            // release covers that EventId. Nothing retries it if no new events arrive.
            // A later ReleaseEvents will release this EventId, too.
            IO(logger.warn(s"ReleaseEvents($eventId) => $problem"))
          case Right(()) =>
            state.update: s =>
              if s.subagentRunId.contains(subagentRunId) then
                s.copy(released = s.released max eventId)
              else
                s
      .compile.drain
      .handleErrorWith: t =>
        IO(logger.error(s"EventReleaser($subagentRunId) => ${t.toStringWithCauses}", t))

  override def toString = s"EventReleaser($subagentId)"


private object EventReleaser:

  def apply(subagentId: SubagentId): IO[EventReleaser] =
    for
      state <- SignallingRef[IO].of(State())
      fiber <- Ref[IO].of(Option.empty[FiberIO[Unit]])
    yield
      new EventReleaser(subagentId, state, fiber)

  private final case class State(
    subagentRunId: Option[SubagentRunId] = None,
    requested: EventId = EventId.BeforeFirst,
    released: EventId = EventId.BeforeFirst)
