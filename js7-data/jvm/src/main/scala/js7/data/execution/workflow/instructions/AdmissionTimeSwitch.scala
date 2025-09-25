package js7.data.execution.workflow.instructions

import cats.effect.std.Dispatcher
import cats.effect.{IO, Ref}
import java.time.ZoneId
import js7.base.time.AdmissionTimeSchemeForJavaTime.*
import js7.base.time.{AdmissionTimeScheme, AlarmClock, NonEmptyTimeInterval, TimeInterval, Timestamp}
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.job.JobKey
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.FiniteDuration

/** Mutable state for calculating the current or next admission time. */
final class AdmissionTimeSwitch(
  admissionTimeScheme: AdmissionTimeScheme,
  findTimeIntervalLimit: FiniteDuration,
  zone: ZoneId,
  jobKey: JobKey,
  onSwitch: Option[TimeInterval] => IO[Unit])
  (using dispatcher: Dispatcher[IO], clock: AlarmClock):

  private given ZoneId = zone

  private val label = s"AdmissionTimeSwitch($jobKey)"
  @volatile private var _nextTime: Option[Timestamp] = None
  private val cancelSchedule = Ref.unsafe[IO, IO[Unit]](IO.unit)

  @TestOnly
  private[instructions] def nextTime = _nextTime

  def cancelDelay: IO[Unit] =
    IO.defer:
      _nextTime = None
      cancelSchedule.get.flatten

  /** Update the state with the current or next admission time and set a _fiber.
   * @return The now effective AdmissionTimeInterval or None. */
  def updateAndCheck(onAdmissionStart: IO[Unit])
  : IO[Option[NonEmptyTimeInterval]] =
    clock.lockIO: now =>
      findCurrentTimeInterval(now) match
        case None =>
          cancelSchedule.getAndSet(IO.unit).flatten
            .as(None) // No admission now or in the future

        case Some(interval) =>
          IO.unlessA(_nextTime.contains(interval.start)):
            onSwitch((interval != TimeInterval.Never) ? interval) *>
              // Also start a fiber if clock has been adjusted
              IO.whenA(now < interval.start):
                _nextTime = Some(interval.start)
                clock.scheduleIOAt(interval.start, label = label):
                  IO.defer:
                    _nextTime = None
                    onAdmissionStart
                .flatMap: cancel =>
                  cancelSchedule.getAndSet(cancel).flatten
          .as:
            // Has admission now?
            interval.contains(now) ? interval.match
              case o: TimeInterval.Standard => o
              case o: TimeInterval.Always => o
              case o: TimeInterval.Never =>
                throw new AssertionError(s"NonEmptyTimeInterval expected, but not: $o")

  def findCurrentTimeInterval(now: Timestamp): Option[TimeInterval] =
    admissionTimeScheme.findLongTimeInterval(
      now, limit = findTimeIntervalLimit, dateOffset = ExecuteExecutor.noDateOffset)

  override def toString = label
