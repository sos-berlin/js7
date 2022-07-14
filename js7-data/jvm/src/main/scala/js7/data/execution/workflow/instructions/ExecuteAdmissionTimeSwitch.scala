package js7.data.execution.workflow.instructions

import java.time.ZoneId
import js7.base.time.AdmissionTimeSchemeForJavaTime.*
import js7.base.time.{AdmissionTimeScheme, AlarmClock, TimeInterval, Timestamp}
import js7.base.utils.ScalaUtils.syntax.*
import monix.execution.cancelables.SerialCancelable
import org.jetbrains.annotations.TestOnly

/** Mutable state for calculating the current or next admission time. */
final class ExecuteAdmissionTimeSwitch(
  admissionTimeScheme: AdmissionTimeScheme,
  zone: ZoneId,
  onSwitch: Option[TimeInterval] => Unit)
{
  @volatile private var _nextTime: Option[Timestamp] = None
  private val timer = SerialCancelable()

  @TestOnly
  private[instructions] def nextTime = _nextTime

  /** Cancel the callback timer for admission start. */
  def cancel(): Unit = {
    _nextTime = None
    timer.cancel()
  }

  /** Update the state with the current or next admission time and set a timer.
   * @return true iff an AdmissionTimeInterval is effective now. */
  def updateAndCheck(onPermissionStart: => Unit)(implicit clock: AlarmClock): Boolean =
    clock.lock {
      val now = clock.now()
      val interval = admissionTimeScheme.findTimeInterval(now, zone,
        dateOffset = ExecuteExecutor.noDateOffset)
      interval match {
        case None =>
          timer.cancel()
          false // Not enterable now

        case Some(interval) =>
          if (!_nextTime.contains(interval.start)) {
            onSwitch((interval != TimeInterval.never) ? interval)
            // Also set timer if clock has been adjusted
            if (now < interval.start) {
              _nextTime = Some(interval.start)
              timer := clock.scheduleAt(interval.start) {
                _nextTime = None
                onPermissionStart
              }
            }
          }

          interval.contains(now) // Has admission now?
      }
    }
}
