package js7.tests.jobs

import cats.effect.ExitCase
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax._
import js7.data.order.Outcome
import js7.launcher.OrderProcess
import js7.launcher.internal.InternalJob
import js7.tests.jobs.SemaphoreJob._
import monix.catnap.Semaphore
import monix.eval.Task
import monix.execution.Scheduler
import scala.reflect.ClassTag

abstract class SemaphoreJob(semaphore: Task[Semaphore[Task]])
extends InternalJob
{
  def this(companion: SemaphoreJob.Companion[_ <: SemaphoreJob]) =
    this(companion.semaphore)

  final def toOrderProcess(step: Step) = {
    val orderId = step.order.id
    OrderProcess(
      step.outTaskObserver.send("STARTED\n")
        .*>(semaphore
          .tapEval(sema =>
            sema.count.flatMap(count =>
              Task(logger.debug(s"$orderId acquire ... (count=$count)"))))
          .flatMap(_.acquire)
          .logWhenItTakesLonger(s"${getClass.getSimpleName}.semaphore.acquire/${step.order.id}")
          .tapEval(_ => Task(logger.debug(s"$orderId acquired")))
          .as(Outcome.succeeded))
    .guaranteeCase {
      case ExitCase.Completed => Task.unit
      case exitCase => Task(logger.warn(s"$orderId $exitCase"))
    })
  }
}

object SemaphoreJob
{
  private val logger = Logger[this.type]

  abstract class Companion[I <: SemaphoreJob](implicit classTag: ClassTag[I])
  extends InternalJob.Companion[I]
  {
    val semaphore = Semaphore[Task](0).memoize

    def reset()(implicit s: Scheduler): Unit = {
      logger.debug("reset")
      (for {
        sema <- semaphore
        count <- sema.count
        _ <-
          if (count > 0) sema.acquireN(count)
          else if (count < 0) sema.releaseN(-count)
          else Task.pure(sema)
      } yield ())
        .runSyncUnsafe()
    }

    def continue(n: Int = 1)(implicit s: Scheduler): Unit = {
      logger.debug(s"continue($n)")
      semaphore
        .flatMap(_.releaseN(n))
        .runSyncUnsafe()
    }

    def continueN(n: Int)(implicit s: Scheduler): Unit = {
      logger.debug(s"continue($n)")
      semaphore
        .flatMap(_.releaseN(n))
        .runSyncUnsafe()
    }
  }
}
