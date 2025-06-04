package js7.data.execution.workflow.instructions

import cats.effect.std.Dispatcher
import cats.effect.{IO, Ref}
import java.time.ZoneId
import js7.base.time.AdmissionTimeSchemeForJavaTime.*
import js7.base.time.{AdmissionTimeScheme, AlarmClock, TimeInterval, Timestamp}
import js7.base.utils.ScalaUtils.syntax.*
import org.jetbrains.annotations.TestOnly

/** Mutable state for calculating the current or next admission time. */
final class ExecuteAdmissionTimeSwitch(
  admissionTimeScheme: AdmissionTimeScheme,
  zone: ZoneId,
  onSwitch: Option[TimeInterval] => IO[Unit])
  (using dispatcher: Dispatcher[IO]):

  @volatile private var _nextTime: Option[Timestamp] = None
  private val cancelSchedule = Ref.unsafe[IO, IO[Unit]](IO.unit)

  @TestOnly
  private[instructions] def nextTime = _nextTime

  /** Cancel the callback _fiber for admission start. */
  def cancel: IO[Unit] =
    IO.defer:
      _nextTime = None
      cancelSchedule.get.flatten

  def cancelDelay: IO[Unit] =
    cancel // TODO Cancel only delay, but not onAdmissionStart

  /** Update the state with the current or next admission time and set a _fiber.
   * @return true iff an AdmissionTimeInterval is effective now. */
  def updateAndCheck(onAdmissionStart: IO[Unit])(using clock: AlarmClock): IO[Boolean] =
    IO.defer:
      val now = clock.now()
      admissionTimeScheme.findTimeInterval(now, zone, dateOffset = ExecuteExecutor.noDateOffset)
      match
        case None =>
          cancelSchedule.getAndSet(IO.unit).flatten
            .as(false) // No admission now or in the future

        case Some(interval) =>
          IO.unlessA(_nextTime.contains(interval.start)):
            onSwitch((interval != TimeInterval.never) ? interval) *>
              // Also start a fiber if clock has been adjusted
              IO.whenA(now < interval.start):
                _nextTime = Some(interval.start)
                clock.scheduleIOAt(interval.start, label = "ExecuteAdmissionTimeSwitch"):
                  IO.defer:
                    _nextTime = None
                    onAdmissionStart
                .flatMap: cancel =>
                  cancelSchedule.getAndSet(cancel).flatten
          .as:
            interval.contains(now) // Has admission now?
