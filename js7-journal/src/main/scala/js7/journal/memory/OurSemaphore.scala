package js7.journal.memory

import cats.effect.std.AtomicCell
import cats.effect.{Deferred, IO, Poll}
import js7.journal.memory.OurSemaphore.*

/** Allows acquisition of any number as soon as the semaphore is completely released.
  *
  * Waiters are served in FIFO order.
  *
  *  (Without FIFO, a request bigger then size could starve.)
  */
private final class OurSemaphore private(size: Int, status: AtomicCell[IO, Status]):

  /** Acquire `n` permits. Only the waiting is cancelable.
    *
    * This method itself does not lose permits. But after it has returned, the caller
    * may be canceled before it takes responsibility for the permits, and then they are lost.
    * Use `acquireN(n, poll)` inside the caller's `IO.uncancelable` to close this gap.
    *
    * For tests and simple usage only. */
  def acquireN(n: Int): IO[Unit] =
    IO.uncancelable: poll =>
      acquireN(n, poll)

  /** Acquire `n` permits. Only the waiting is cancelable.
    *
    * Must be called inside `IO.uncancelable(poll => ...)`.
    * On return, the permits are held by the caller, without a cancellation gap
    * until the caller's uncancelable region ends.
    * When canceled while waiting, no permits are held. */
  def acquireN(n: Int, poll: Poll[IO]): IO[Unit] =
    status.evalModify: status =>
      if status.queue.isEmpty && isAvailable(n, status) then
        IO.pure:
          status.copy(count = status.count - n) -> IO.unit
      else
        Deferred[IO, Unit].map: deferred =>
          val waiting = Waiting(n, deferred)
          status.enqueue(waiting) ->
            poll:
              deferred.get
            .onCancel:
              cancelWaiting(waiting)
    .flatten // Wait outside of evalModify (still in the caller's uncancelable region)

  private def cancelWaiting(waiting: Waiting): IO[Unit] =
    status.evalUpdate: status =>
      if status.queue.exists(_ eq waiting) then
        // Still waiting. Remove from queue, the next waiter may be satisfiable now
        wakeUp(status.copy(queue = status.queue.filterNot(_ eq waiting)))
      else
        // releaseN has already granted the permits. Give them back
        wakeUp(status.copy(count = status.count + waiting.requested))

  def releaseN(n: Int): IO[Unit] =
    status.evalUpdate: status =>
      wakeUp(status.copy(count = status.count + n))

  /** Grant permits to the waiters in FIFO order, as long as available. */
  private def wakeUp(status: Status): IO[Status] =
    status.queue.headOption match
      case Some(waiting) if isAvailable(waiting.requested, status) =>
        waiting.deferred.complete(()) *>
          wakeUp(status.copy(
            count = status.count - waiting.requested,
            queue = status.queue.tail))
      case _ =>
        IO.pure(status)

  /** Returns a negative value when more than size has been acquired.
    *
    * This differs from cats.effect.std.Semaphore. */
  def available: IO[Int] =
    status.get.map: status =>
      status.count

  private def isAvailable(requested: Int, status: Status): Boolean =
    requested <= status.count || status.count == size


private object OurSemaphore:

  def apply(size: Int): IO[OurSemaphore] =
    for
      status <- AtomicCell[IO].of(Status(size, Vector.empty))
    yield
      new OurSemaphore(size, status)


  private final case class Status(count: Int, queue: Vector[Waiting]):
    def enqueue(waiting: Waiting): Status =
      copy(queue = queue :+ waiting)


  private final class Waiting(val requested: Int, val deferred: Deferred[IO, Unit]):
    override def toString = s"Waiting($requested)"
