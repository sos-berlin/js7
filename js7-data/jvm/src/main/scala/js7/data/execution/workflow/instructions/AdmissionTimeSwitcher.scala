package js7.data.execution.workflow.instructions

import cats.effect.kernel.Resource
import cats.effect.std.Dispatcher
import cats.effect.{IO, ResourceIO}
import fs2.concurrent.{Signal, SignallingRef}
import java.time.ZoneId
import js7.base.catsutils.CatsExtensions.*
import js7.base.log.Logger
import js7.base.service.Service
import js7.base.time.{AdmissionTimeScheme, AlarmClock, TimeInterval}
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.execution.workflow.instructions.AdmissionTimeSwitch
import js7.data.execution.workflow.instructions.AdmissionTimeSwitcher.*
import js7.data.job.JobKey
import scala.concurrent.duration.FiniteDuration

final class AdmissionTimeSwitcher private(
  currentAdmissionTimeSignal: SignallingRef[IO, Option[TimeInterval]],
  admissionTimeScheme: AdmissionTimeScheme,
  zoneId: ZoneId,
  findTimeIntervalLimit: FiniteDuration,
  jobKey: JobKey)
  (using clock: AlarmClock, dispatcher: Dispatcher[IO])
extends
  Service.StoppableByRequest:

  private val admissionTimeSwitch =
    AdmissionTimeSwitch(admissionTimeScheme, findTimeIntervalLimit, zoneId, jobKey,
      onSwitch = to =>
        IO:
          if !to.contains(TimeInterval.Always) then
            logger.debug(s"$jobKey: Next admission: ${to getOrElse "None"} $zoneId"))

  /** Signal for the currently valid admission TimeInterval.
    *
    * Or None when no admission time is currently valid (i.e. the door is closed).
    * Every change is signalled. */
  def admissionSignal: Signal[IO, Option[TimeInterval]] =
    currentAdmissionTimeSignal

  protected def start =
    // First Signal will be duplicated by selectTimeInterval — TODO optimise this
    clock.nowIO.flatMap: now =>
      currentAdmissionTimeSignal.set:
        admissionTimeSwitch.findCurrentTimeInterval(now)
    *>
      startService:
        selectTimeIntervalAgainAndAgain.background.surround:
          untilStopRequested
        *>
          admissionTimeSwitch.cancelDelay

  private val selectTimeIntervalAgainAndAgain: IO[Unit] =
    selectTimeInterval(IO.defer(selectTimeIntervalAgainAndAgain))

  private def selectTimeInterval(onPermissionStartOrEnd: IO[Unit]): IO[Unit] =
    IO(isStopping).ifFalse: // clock.sleepUntil may be not cancelable
      admissionTimeSwitch.updateAndCheck:
        onPermissionStartOrEnd
      .flatMap: maybeTimeInterval =>
        logger.trace(s"selectTimeInterval $jobKey ${maybeTimeInterval getOrElse "None"}")
        currentAdmissionTimeSignal.set(maybeTimeInterval) *>
          maybeTimeInterval.foldMap: timeInterval =>
            clock.sleepUntil(timeInterval.end) *>
              onPermissionStartOrEnd

  override def toString = s"AdmissionTimeSwitcher($jobKey)"


object AdmissionTimeSwitcher:
  private val logger = Logger[this.type]

  def service(
    admissionTimeScheme: AdmissionTimeScheme,
    zoneId: ZoneId,
    findTimeIntervalLimit: FiniteDuration,
    jobKey: JobKey)
    (using AlarmClock, Dispatcher[IO])
  : ResourceIO[AdmissionTimeSwitcher] =
    for
      admissionSignal <- Resource.eval:
        SignallingRef[IO].of[Option[TimeInterval]](None)
      service <- Service.resource:
        new AdmissionTimeSwitcher(
          admissionSignal, admissionTimeScheme, zoneId, findTimeIntervalLimit, jobKey)
    yield
      service

  /** Return a Service if admissionTimeScheme, otherwise a dummy signal is returned. */
  def signalService(
    admissionTimeScheme: Option[AdmissionTimeScheme],
    zoneId: ZoneId,
    findTimeIntervalLimit: FiniteDuration,
    jobKey: JobKey)
    (using AlarmClock, Dispatcher[IO])
  : ResourceIO[Signal[IO, Option[TimeInterval]]] =
    admissionTimeScheme match
      case None =>
        Resource.eval:
          SignallingRef[IO].of(Some(TimeInterval.Always))

      case Some(admissionTimeScheme) =>
        service(admissionTimeScheme, zoneId, findTimeIntervalLimit, jobKey)
          .map(_.admissionSignal)
