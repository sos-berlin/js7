package js7.tests.jobs

import js7.data.order.Outcome
import js7.launcher.internal.InternalJob

final class FailingJob extends EmptyJob(Outcome.failed)

object FailingJob extends InternalJob.Companion[FailingJob]
