package js7.data.execution.workflow.instructions

import java.time.ZoneId
import js7.base.monixlike.SerialSyncCancelable
import js7.base.time.AdmissionTimeSchemeForJavaTime.*
import js7.base.time.{AdmissionTimeScheme, AlarmClock, TimeInterval, Timestamp}
import js7.base.utils.ScalaUtils.syntax.*
import org.jetbrains.annotations.TestOnly

/** Mutable state for calculating the current or next admission time. */
final class ExecuteAdmissionTimeSwitch(
  admissionTimeScheme: AdmissionTimeScheme,
  zone: ZoneId,
  onSwitch: Option[TimeInterval] => Unit):

  private given ZoneId = zone

  @volatile private var _nextTime: Option[Timestamp] = None
  private val _timer = SerialSyncCancelable()

  @TestOnly
  private[instructions] def nextTime = _nextTime

  /** Cancel the callback _timer for admission start. */
  def cancel(): Unit =
    _nextTime = None
    _timer.cancel()

  /** Update the state with the current or next admission time and set a _timer.
   * @return true iff an AdmissionTimeInterval is effective now. */
  def updateAndCheck(onAdmissionStart: => Unit)(using clock: AlarmClock): Boolean =
    clock.lock:
      val now = clock.now()
      admissionTimeScheme.findTimeInterval(now, dateOffset = ExecuteExecutor.noDateOffset)
      match
        case None =>
          _timer.cancel()
          false // Not enterable now

        case Some(interval) =>
          if !_nextTime.contains(interval.start) then
            onSwitch((interval != TimeInterval.never) ? interval)
            // Also set _timer if clock has been adjusted
            if now < interval.start then
              _nextTime = Some(interval.start)
              _timer := clock.scheduleAt(interval.start):
                _nextTime = None
                onAdmissionStart
          interval.contains(now) // Has admission now?
