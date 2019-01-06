package com.sos.jobscheduler.common.akkahttp.web.session

import com.sos.jobscheduler.base.time.Timestamp
import java.util.concurrent.TimeUnit.MILLISECONDS
import monix.execution.Scheduler
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
trait HasTimeout
{
  @volatile
  private var timeoutAt: Long = Long.MaxValue

  final def isAlive(implicit scheduler: Scheduler) =
    scheduler.clockRealTime(MILLISECONDS) < timeoutAt

  private[session] final def touch(timeout: FiniteDuration)(implicit scheduler: Scheduler): Unit =
    timeoutAt = scheduler.currentTimeMillis() + timeout.toMillis

  private[session] final def isEternal =
    timeoutAt == Long.MaxValue

  final def touchedAt =
    Timestamp.ofEpochMilli(timeoutAt)
}
