package js7.tests.jobs

import js7.data.order.Outcome
import js7.launcher.OrderProcess
import js7.launcher.internal.InternalJob
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
    OrderProcess(
      semaphore
        .flatMap(_.acquire)
        .as(Outcome.succeeded))
  }
}

object SemaphoreJob
{
  abstract class Companion[I <: SemaphoreJob](implicit classTag: ClassTag[I])
  extends InternalJob.Companion[I]
  {
    val semaphore = Semaphore[Task](0).memoize

    def continue(n: Int = 1)(implicit s: Scheduler): Unit =
      semaphore.flatMap(_.releaseN(n)).runSyncUnsafe()

    def continueN(n: Int)(implicit s: Scheduler): Unit =
      semaphore.flatMap(_.releaseN(n)).runSyncUnsafe()
  }
}
