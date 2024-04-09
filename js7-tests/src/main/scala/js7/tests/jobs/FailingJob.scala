package js7.tests.jobs

import js7.data.order.OrderOutcome
import js7.launcher.internal.InternalJob

final class FailingJob extends EmptyJob(FailingJob.outcome)


object FailingJob extends InternalJob.Companion[FailingJob]:
  val outcome: OrderOutcome.Failed =
    OrderOutcome.Failed(Some("💥FailingJob failed💥"))
