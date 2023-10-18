package js7.base.service

import StoppableByRequest._
import cats.effect.concurrent.Deferred
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.MonixBase.syntax.RichMonixTask
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import monix.eval.{Fiber, Task}

trait StoppableByRequest:
  private final val fiber = Deferred.unsafe[Task, Fiber[Unit]]
  private val stopRequested = Deferred.unsafe[Task, Unit]
  @volatile private var _isStopping = false

  protected final def isStopping: Boolean =
    _isStopping

  private[service] final def onFiberStarted(fiber: Fiber[Unit]): Task[Unit] =
    this.fiber.complete(fiber)

  protected final def untilStopRequested: Task[Unit] =
    stopRequested.get

  private val memoizedStop =
    Task.defer(
      logger.traceTask(s"$toString stop") {
        _isStopping = true
        stopRequested.complete(())
          .*>(fiber.get)
          .flatMap(_.join)
      }).memoize

  protected def stop: Task[Unit] =
    memoizedStop

  protected final def failWhenStopRequested[A](body: Task[A]): Task[A] =
    body.raceFold(
      untilStopRequested *>
        Task.raiseError(new IllegalStateException(s"$toString is being stopped")))

  protected final def requireNotStopping: Task[Checked[Unit]] =
    Task:
      !isStopping !! Problem(s"$toString is stopping")


object StoppableByRequest:
  private val logger = Logger[this.type]
