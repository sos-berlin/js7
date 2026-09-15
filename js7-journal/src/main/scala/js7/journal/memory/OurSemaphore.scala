package js7.journal.memory

import cats.effect.IO
import cats.effect.kernel.Deferred
import cats.effect.std.AtomicCell
import js7.journal.memory.OurSemaphore.*

/** Allows acquisition of any number as soon as the sempaphore is completely released. */
private final class OurSemaphore private(size: Int, status: AtomicCell[IO, Status]):

  def acquireN(n: Int): IO[Unit] =
    status.evalModify: status =>
      if isAvailable(n, status) then
        IO.pure:
          status.copy(count = status.count - n) -> IO.unit
      else
        for
          deferred <- Deferred[IO, Unit]
          updatedStatus = status.enqueue(Waiting(n, deferred))
        yield
          updatedStatus -> deferred.get
    .flatten // wait outside of evalModify

  def releaseN(n: Int): IO[Unit] =
    status.evalUpdate: origStatus =>
      val status = origStatus.copy(count = origStatus.count + n)
      status.queue.headOption match
        case Some(waiting) if isAvailable(waiting.requested, status) =>
          waiting.deferred.complete(()).as:
            status.copy(
              count = status.count - waiting.requested,
              queue = status.queue.tail)
        case _ =>
          IO.pure(status)

  /** Returns a negative value when more than size has been acquired.
    *
    * This defers from cats.effect.std.Semaphore. */
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
