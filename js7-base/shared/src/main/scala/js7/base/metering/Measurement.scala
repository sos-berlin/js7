package js7.base.metering

import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.*
import js7.base.utils.ScalaUtils.syntax.*
import scala.concurrent.duration.*

/** Snapshot of a CallMeter. */
private[metering] final case class Measurement(
  callMeter: CallMeter,
  elapsedNanos: Long,
  count: Long,
  countedNanos: Long):

  def duration: FiniteDuration =
    countedNanos.ns

  /** Quotient of metered and elapsed time — may be NaN. */
  def quote: Double =
    if elapsedNanos == 0 then
      Double.NaN
    else
      countedNanos.toDouble / elapsedNanos

  def name: String =
    callMeter.name

  def diff(m: Measurement): Measurement =
    Measurement(callMeter,
      elapsedNanos = elapsedNanos - m.elapsedNanos,
      count = count - m.count,
      countedNanos = countedNanos - m.countedNanos)

  def copy(): Measurement =
    Measurement(callMeter, elapsedNanos, count, countedNanos)

  def asString: String =
    synchronized:
      val pct = (elapsedNanos > 0) ?? f" ${100.0 * countedNanos / elapsedNanos}%6.1f%%"
      f"$count%9d×${(count > 0) ?? s" ${(countedNanos / count).ns.pretty}"}%8s ${
        countedNanos.ns.pretty
      }%8s/${elapsedNanos.ns.pretty}%-8s$pct ${callMeter.name}"
