package js7.tests.jobs

import js7.launcher.OrderProcess
import js7.launcher.internal.InternalJob

final class EmptyJob extends InternalJob
{
  def toOrderProcess(step: Step) =
    OrderProcess.succeeded()
}

object EmptyJob extends InternalJob.Companion[EmptyJob]
