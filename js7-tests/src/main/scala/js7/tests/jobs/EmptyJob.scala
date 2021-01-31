package js7.tests.jobs

import js7.data.job.internal.InternalJob
import js7.data.value.NamedValues
import monix.eval.Task

final class EmptyJob
extends InternalJob
{
  def processOrder(context: InternalJob.OrderContext) =
    Task.pure(Right(NamedValues.empty))
}
