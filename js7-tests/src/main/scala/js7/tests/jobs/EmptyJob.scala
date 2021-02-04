package js7.tests.jobs

import js7.data.value.NamedValues
import js7.executor.internal.InternalJob
import js7.executor.internal.InternalJob.{OrderProcess, OrderContext, Result}
import monix.eval.Task

final class EmptyJob extends InternalJob
{
  def processOrder(context: OrderContext) =
    OrderProcess(
      Task.pure(Right(
        Result(NamedValues.empty))))
}
