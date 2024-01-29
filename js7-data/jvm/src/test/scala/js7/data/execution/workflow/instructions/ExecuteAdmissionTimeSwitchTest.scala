package js7.data.execution.workflow.instructions

import cats.effect.IO
import cats.effect.testkit.TestControl
import cats.effect.unsafe.Scheduler
import java.time.DayOfWeek.{MONDAY, SUNDAY}
import java.time.ZoneOffset.UTC
import java.time.{LocalTime, ZoneId}
import js7.base.catsutils.CatsEffectExtensions.unsafeScheduler
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.test.OurAsyncTestSuite
import js7.base.time.JavaTimestamp.local
import js7.base.time.JavaTimestamp.specific.RichJavaTimestamp
import js7.base.time.ScalaTime.*
import js7.base.time.{AdmissionTimeScheme, AlarmClock, TestAlarmClock, TimeInterval, Timestamp, WeekdayPeriod}
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.execution.workflow.instructions.ExecuteAdmissionTimeSwitchTest.*
import org.scalatest.Assertion
import org.scalatest.Assertions.assert
import scala.concurrent.duration.*

final class ExecuteAdmissionTimeSwitchTest extends OurAsyncTestSuite:

  //private given Scheduler = ioRuntime.scheduler

  "AdmissionTimeScheme.always" in:
    val now = Timestamp("2021-01-01T12:00:00Z")
    val switch = new ExecuteAdmissionTimeSwitch(AdmissionTimeScheme.always, UTC, _ => ())

    TestControl.executeEmbed:
      IO.unsafeRuntime.flatMap: ioRuntime =>
        given Scheduler = ioRuntime.scheduler
        given alarmClock: AlarmClock = TestAlarmClock(now)
        for
          _ <- switch.updateAndCheck(sys.error("FAILED"))
          _ = assert(switch.nextTime == None)
          isEnterable <- switch.updateAndCheck(sys.error("FAILED"))
          _ = assert(isEnterable)
          _ = assert(switch.nextTime == None)
          _ <- IO.sleep(99*365*24.h)
          isEnterable <- switch.updateAndCheck(sys.error("FAILED"))
        yield
          assert(isEnterable)

  "UTC" in:
    TestControl.executeEmbed:
      check(ZoneId.of("UTC"))

  // --> Mariehamn switched to daylight saving time at 2021-03-28 03:00 <-- !!!
  // Time zone offset changes from +02:00 to +03:00
  private val mariehamnZoneId = ZoneId.of("Europe/Mariehamn")

  "Europe/Mariehamn" - {
    "Daylight saving time" in:
      TestControl.executeEmbed:
        implicit val zone = mariehamnZoneId
        assert(local("2021-03-28T02:59") == Timestamp("2021-03-28T00:59:00Z"))
        assert(local("2021-03-28T04:00") == Timestamp("2021-03-28T01:00:00Z"))
        assert(local("2021-03-28T04:00") - local("2021-03-28T02:59:59") == 1.s)
        check(zone)

    given zone: ZoneId = mariehamnZoneId
    val admissionTimeScheme = AdmissionTimeScheme(Seq(
      WeekdayPeriod(SUNDAY, LocalTime.of(3, 0), 1.h)))

    "Week without a period due to daylight saving time gap" in:
      TestControl.executeEmbed:
        for
          tester <- Tester(admissionTimeScheme)
          _ <-
            tester.check(local("2021-03-21T00:00"), admissionStarted = false, isAdmitted = false,
              switched = Some(TimeInterval(local("2021-03-21T03:00"), 1.h)))
          _ <-
            tester.check(
              local("2021-03-21T03:00"), admissionStarted = true, isAdmitted = true)

          // Because the admission is in the dst gap, the next valid local time is returned.
          _ <-
            tester.check(
              local("2021-03-21T04:00"), admissionStarted = false, isAdmitted = false,
              switched = Some(TimeInterval(local("2021-03-28T04:00"), 1.h)))

          // Admission time 03:00 until 04:00, while clock is skipping this hour.
          // So the admission time ends when it starts.
          // Therefore, the next admission time is selected.
          _ <-
            tester.check(
              local("2021-03-28T04:00"), admissionStarted = true, isAdmitted = true)
          _ <-
            tester.check(
              local("2021-03-28T04:59"), admissionStarted = false, isAdmitted = true)
          _ <-
            tester.check(
              local("2021-03-28T05:00"), admissionStarted = false, isAdmitted = false,
              switched = Some(TimeInterval(local("2021-04-04T03:00"), 1.h)))
        yield
          succeed
  }

  private def check(zone: ZoneId): IO[Assertion] =
    IO.unsafeScheduler.flatMap: scheduler =>
      given Scheduler = scheduler
      given ZoneId = zone
      val admissionTimeScheme = AdmissionTimeScheme(Seq(
        WeekdayPeriod(MONDAY, LocalTime.of(8, 0), 2.h),
        WeekdayPeriod(SUNDAY, LocalTime.of(23, 0), 3.h)))  // Until Monday 2:00

      assert(local("2021-03-22T00:00").toLocalDateTime(zone).getDayOfWeek == MONDAY)
      assert(local("2021-03-28T00:00").toLocalDateTime(zone).getDayOfWeek == SUNDAY)
      val tester = new Tester(admissionTimeScheme)

      for
        _ <-
          tester.check( // Monday
            local("2021-03-22T00:00"), admissionStarted = false, isAdmitted = true,
            switched = Some(TimeInterval(local("2021-03-21T23:00"), 3.h)))
        _ <-
          tester.check(
            local("2021-03-22T07:00"), admissionStarted = false, isAdmitted = false,
            switched = Some(TimeInterval(local("2021-03-22T08:00"), 2.h)))
        _ <-
          tester.check(
            local("2021-03-22T08:00"), admissionStarted = true, isAdmitted = true)
        _ <-
          tester.check(
            local("2021-03-22T09:59"), admissionStarted = false, isAdmitted = true)
        _ <-
          tester.check(
            local("2021-03-22T10:00"), admissionStarted = false, isAdmitted = false,
            switched = Some(TimeInterval(local("2021-03-28T23:00"), 3.h)))
        _ <-
          tester.check( // Sunday
            local("2021-03-28T23:00"), admissionStarted = true, isAdmitted = true)
      yield
        succeed


object ExecuteAdmissionTimeSwitchTest:

  private val logger = Logger[this.type]

  private final class Tester(admissionTimeScheme: AdmissionTimeScheme)
    (using zone: ZoneId, scheduler: Scheduler):
    private given clock: TestAlarmClock = TestAlarmClock(Timestamp.Epoch)
    private var _switched: Option[TimeInterval] = null
    private val switch = new ExecuteAdmissionTimeSwitch(admissionTimeScheme, zone,
      onSwitch = _switched = _)
    private var admissionStarts = 0

    def check(
      now: Timestamp,
      admissionStarted: Boolean,
      isAdmitted: Boolean,
      switched: Option[TimeInterval] = None)
    : IO[Assertion] =
      logger.debugIO("check", s"$now admissionStarted=$admissionStarted isAdmitted=$isAdmitted switched=$switched"):
        IO.defer:
          val was = admissionStarts
          val sw = switched.fold(_switched)(Some(_))
          for
            _ <- clock.set(now)
            isAdmitted_ <- switch.updateAndCheck:
              admissionStarts += 1
              logger.debug(s"onAdmissionStart: admissionStarts=$admissionStarts")
            _ <- IO.sleep(1.ns) // Allow callback above to run now
          yield
            assert(_switched == sw &&
              isAdmitted_ == isAdmitted &&
              admissionStarts == was + admissionStarted.toInt)

  private object Tester:
    def apply(admissionTimeScheme: AdmissionTimeScheme)(using zone: ZoneId): IO[Tester] =
      for scheduler <- IO.unsafeScheduler yield
        given Scheduler = scheduler
        new Tester(admissionTimeScheme)
