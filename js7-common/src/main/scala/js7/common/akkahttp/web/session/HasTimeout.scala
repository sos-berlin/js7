package js7.common.akkahttp.web.session

import js7.base.time.Timestamp
import monix.execution.Scheduler
import scala.concurrent.duration.*

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
    timeoutAt = scheduler.clockRealTime(MILLISECONDS) + timeout.toMillis

  private[session] final def isEternal =
    timeoutAt == Long.MaxValue

  final def touchedAt =
    Timestamp.ofEpochMilli(timeoutAt)
}
