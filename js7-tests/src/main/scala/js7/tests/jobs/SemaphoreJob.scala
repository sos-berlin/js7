package js7.tests.jobs

import js7.data.order.Outcome
import js7.executor.OrderProcess
import js7.executor.internal.InternalJob
import monix.catnap.Semaphore
import monix.eval.Task
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
    private[SemaphoreJob] val semaphore = Semaphore[Task](0).memoize

    def continue: Task[Unit] =
      semaphore.flatMap(_.release)//.runSyncUnsafe()
  }
}
