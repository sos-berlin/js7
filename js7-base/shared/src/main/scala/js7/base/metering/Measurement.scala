package js7.base.metering

import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.*
import js7.base.utils.ScalaUtils.syntax.*
import scala.concurrent.duration.*

/** Snapshot of a CallMeter. */
private[metering] final case class Measurement(
  callMeter: CallMeter,
  total: Long,
  active: Int,
  meteredNanos: Long,
  elapsedNanos: Long):

  def duration: FiniteDuration =
    meteredNanos.ns

  /** Quotient of metered and elapsed time — may be NaN. */
  def quote: Double =
    if elapsedNanos == 0 then
      Double.NaN
    else
      meteredNanos.toDouble / elapsedNanos

  def name: String =
    callMeter.name

  def diff(m: Measurement): Measurement =
    Measurement(callMeter,
      total = total - m.total,
      active = active,
      meteredNanos = meteredNanos - m.meteredNanos,
      elapsedNanos = elapsedNanos - m.elapsedNanos)

  def copy(): Measurement =
    Measurement(callMeter, total, active, meteredNanos, elapsedNanos)

  def asString: String =
    synchronized:
      val pct = (elapsedNanos > 0) ?? f" ${100.0 * meteredNanos / elapsedNanos}%6.1f%%"
      f"$total%9d×${(total > 0) ?? s" ${(meteredNanos / total).ns.pretty}"}%8s ${
        meteredNanos.ns.pretty
      }%8s/${elapsedNanos.ns.pretty}%-8s$pct ${callMeter.name}"
