package js7.tests.jobs

import cats.syntax.traverse._
import js7.base.time.ScalaTime.RichFiniteDuration
import js7.base.utils.ScalaUtils.syntax._
import js7.data.order.Outcome
import js7.launcher.OrderProcess
import js7.launcher.internal.InternalJob
import js7.launcher.internal.InternalJob.JobContext
import monix.eval.Task

final class SleepJob(jobContext: JobContext) extends InternalJob
{
  import jobContext.clock

  def toOrderProcess(step: Step) =
    OrderProcess(
      step.arguments
        .checked("sleep")
        .flatMap(_.asDuration)
        .traverse(duration =>
          (if (duration.isPositive) clock.sleep(duration) else Task.unit)
            .as(Outcome.succeeded))
        .map(Outcome.Completed.fromChecked))
}

object SleepJob extends InternalJob.Companion[SleepJob]
