package js7.tests.jobs

import js7.executor.OrderProcess
import js7.executor.internal.InternalJob

final class EmptyJob extends InternalJob
{
  def toOrderProcess(step: Step) =
    OrderProcess.succeeded()
}

object EmptyJob extends InternalJob.Companion[EmptyJob]
