package js7.base.log

import implicitbox.Not
import monix.execution.Scheduler
import monix.execution.misc.{CanBindLocals, Local}
import monix.execution.schedulers.TracingScheduler

object CorrelIdBinder
{
  private[log] val local = Local(CorrelId.empty)
  private var currentCorrelIdCount = 0L

  def onLog4jStop(): Unit =
    if (CorrelId.isEnabled) logStatistics()

  def statistics: String =
    CorrelId.statistics + ", " +
    s"${CanBindCorrelId.bindCorrelIdCount}× bindCorrelId, " +
      s"$currentCorrelIdCount× currentCorrelId"

  def logStatistics(): Unit =
    scribe.debug(statistics)

  def currentCorrelId: CorrelId =
    if (!CorrelId.isEnabled)
      CorrelId.empty
    else {
      currentCorrelIdCount += 1
      local()
    }

  private def updateCorrelId(correlId: CorrelId): Unit =
    local.update(correlId)

  def bindCorrelId[R](correlId: CorrelId)(body: => R)(implicit R: CanBindCorrelId[R]): R =
    R.bind(correlId)(body)

  /** For a synchronous non-Unit executable body only, uses `CanBindLocals.synchronous`. */
  def bindCorrelIdNow[R](correlId: CorrelId)(body: => R)(implicit ev: Not[CanBindLocals[R]]): R =
    bindCorrelId(correlId)(body)(CanBindCorrelId.synchronous)

  def enableScheduler(scheduler: Scheduler): Scheduler =
    if (!CorrelId.couldBeEnabled)
      scheduler
    else
      TracingScheduler(scheduler)

  def isolate[R](body: LogCorrelId => R): R =
    if (!CorrelId.isEnabled)
      body(EmptyLogCorrelId)
    else {
      val previous = currentCorrelId
      try body(new ActiveLogCorrelId(previous))
      finally updateCorrelId(previous)
    }

  sealed trait LogCorrelId {
    def :=(correlId: CorrelId): Unit
  }

  private final class ActiveLogCorrelId(private var currCorrelId: CorrelId)
  extends LogCorrelId
  {
    def :=(correlId: CorrelId): Unit =
      if (correlId != currCorrelId) {
        currCorrelId = correlId
        updateCorrelId(correlId)
      }
  }

  private object EmptyLogCorrelId extends LogCorrelId {
    def :=(correlId: CorrelId) = {}
  }
}
