package js7.tests.jobs

import js7.base.log.Logger
import js7.data.order.Outcome
import js7.launcher.OrderProcess
import js7.launcher.internal.InternalJob
import js7.tests.jobs.EmptyJob.logger
import monix.eval.Task

final class EmptyJob extends InternalJob
{
  def toOrderProcess(step: Step) =
    OrderProcess(Task {
      logger.debug(s"EmptyJob ${step.order.id}")
      Outcome.succeeded
    })
}

object EmptyJob extends InternalJob.Companion[EmptyJob]
{
  private val logger = Logger[this.type]
}
