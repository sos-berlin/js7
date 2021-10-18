package js7.base.time

import java.time.ZoneId
import js7.base.time.AdmissionTimeSchemeForJavaTime.JavaAdmissionTimeSchemeJava
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax._
import monix.execution.cancelables.SerialCancelable

/** Mutable state for calculating the current or next admission time. */
final class AdmissionTimeIntervalSwitch(
  admissionTimeScheme: Option[AdmissionTimeScheme],
  onSwitch: (Option[TimeInterval], Option[TimeInterval]) => Unit)
{
  private val admissionTimer = SerialCancelable()
  private var _currentTimeInterval: Option[TimeInterval] = Some(
    if (admissionTimeScheme.isEmpty)
      TimeInterval.alwaysSinceEpoch
    else
      TimeInterval.never/*will be changed on first `update`*/)

  private[time] def currentTimeInterval = _currentTimeInterval
  @volatile private var timerActive = false

  /** Cancel the callback timer for admission start. */
  def cancel(): Unit =
    admissionTimer.cancel()

  /** Update the state with the current or next admission time and set a timer. */
  def update(zone: ZoneId)(onPermissionStart: => Unit)(implicit clock: AlarmClock)
  : Boolean =
    clock.lock {
      val now = clock.now()
      val updated = updateAdmission(now, zone)
      if (updated || !timerActive/*also set timer if clock has been adjusted*/) {
        callbackOnIntervalStart(now, onPermissionStart, clock)
      }
      isEnterable(now)
    }

  private[time] def updateAdmission(now: Timestamp, zone: ZoneId): Boolean =
    admissionTimeScheme.fold(false)(admissionTimeScheme =>
      _currentTimeInterval.fold(false)(admissionInterval =>
        admissionInterval.endsBefore(now) && {
          val next = admissionTimeScheme.findTimeInterval(now, zone, dateOffset = 0.s)
          onSwitch((admissionInterval != TimeInterval.never) ? admissionInterval, next)
          _currentTimeInterval = next
          true
        }))

  private def callbackOnIntervalStart(now: Timestamp, callback: => Unit, clock: AlarmClock) =
    for (interval <- _currentTimeInterval) {
      if (interval.start > now) {
        timerActive = true
        admissionTimer := clock.scheduleAt(interval.start) {
          timerActive = false
          callback
        }
      }
    }

  private def isEnterable(now: Timestamp) =
    _currentTimeInterval.exists(_.contains(now))
}
