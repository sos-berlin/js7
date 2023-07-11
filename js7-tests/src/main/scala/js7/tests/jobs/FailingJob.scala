package js7.tests.jobs

import js7.data.order.Outcome
import js7.launcher.internal.InternalJob

final class FailingJob extends EmptyJob(FailingJob.outcome)

object FailingJob extends InternalJob.Companion[FailingJob] {
  val outcome = Outcome.Failed(Some("💥FailingJob failed💥"))
}
