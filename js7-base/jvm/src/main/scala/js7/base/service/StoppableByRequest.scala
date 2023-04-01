package js7.base.service

import cats.effect.concurrent.Deferred
import monix.eval.{Fiber, Task}

trait StoppableByRequest {
  private final val fiber = Deferred.unsafe[Task, Fiber[Unit]]
  private val stopRequested = Deferred.unsafe[Task, Unit]

  private[service] final def onFiberStarted(fiber: Fiber[Unit]): Task[Unit] =
    this.fiber.complete(fiber)

  protected final def untilStopRequested: Task[Unit] =
    stopRequested.get.uncancelable

  private val memoizedStop =
    stopRequested.complete(())
      .*>(fiber.get)
      .flatMap(_.join)
      .memoize

  protected def stop =
    memoizedStop
}
