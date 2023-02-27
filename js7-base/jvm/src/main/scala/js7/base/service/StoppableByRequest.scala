package js7.base.service

import cats.effect.concurrent.Deferred
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.MonixBase.syntax.RichMonixTask
import js7.base.service.StoppableByRequest.*
import monix.eval.{Fiber, Task}

trait StoppableByRequest {
  private final val fiber = Deferred.unsafe[Task, Fiber[Unit]]
  private val stopRequested = Deferred.unsafe[Task, Unit]

  private[service] final def onFiberStarted(fiber: Fiber[Unit]): Task[Unit] =
    this.fiber.complete(fiber)

  protected final def untilStopRequested: Task[Unit] =
    stopRequested.get

  private val memoizedStop =
    stopRequested.complete(())
      .*>(fiber.get)
      .flatMap(_.join)
      .logWhenItTakesLonger(s"stopping $toString")
      .memoize

  protected def stop =
    logger.debugTask(s"$toString stop")(
      memoizedStop)
}

object StoppableByRequest {
  private val logger = Logger[this.type]
}
