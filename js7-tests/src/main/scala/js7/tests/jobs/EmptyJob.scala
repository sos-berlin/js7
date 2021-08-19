package js7.tests.jobs

import js7.data.order.Outcome
import js7.executor.OrderProcess
import js7.executor.internal.InternalJob
import monix.eval.Task

final class EmptyJob extends InternalJob
{
  def toOrderProcess(step: Step) =
    OrderProcess(Task.pure(Outcome.succeeded))
}

object EmptyJob extends InternalJob.Companion[EmptyJob]
