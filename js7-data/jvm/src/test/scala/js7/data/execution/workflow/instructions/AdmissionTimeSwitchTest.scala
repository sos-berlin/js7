package js7.data.execution.workflow.instructions

import cats.effect.IO
import cats.effect.std.Dispatcher
import cats.effect.unsafe.IORuntime
import java.time.DayOfWeek.{MONDAY, SUNDAY}
import java.time.ZoneOffset.UTC
import java.time.{LocalTime, ZoneId}
import js7.base.test.OurAsyncTestSuite
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.time.JavaTimestamp.local
import js7.base.time.JavaTimestamp.specific.RichJavaTimestamp
import js7.base.time.ScalaTime.*
import js7.base.time.TimestampForTests.ts
import js7.base.time.{AdmissionTimeScheme, TestAlarmClock, TimeInterval, Timestamp, WeekdayPeriod}
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.job.JobKey
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.Position
import js7.tester.ScalaTestUtils.awaitAndAssert
import org.scalatest.Assertion
import scala.concurrent.duration.*


final class AdmissionTimeSwitchTest extends OurAsyncTestSuite:

  private given IORuntime = ioRuntime
  private val jobKey = JobKey(WorkflowPath("WORKFLOW") /: Position(1))

  "AdmissionTimeScheme.always" in:
    Dispatcher.parallel[IO].use: dispatcher =>
      given Dispatcher[IO] = dispatcher
      IO:
        val now = ts"2021-01-01T12:00:00Z"
        given clock: TestAlarmClock = TestAlarmClock(now)
        val switch = AdmissionTimeSwitch(AdmissionTimeScheme.always, 9999.days, UTC, jobKey,
          onSwitch = _ => IO.unit)

        switch.updateAndCheck(IO(sys.error("FAILED"))).await(99.s)
        assert(switch.nextTime == None)

        var timeInterval = switch.updateAndCheck(IO(sys.error("FAILED"))).await(99.s)
        assert(timeInterval.isDefined)
        assert(switch.nextTime == None)

        clock += 99*365*24.h
        timeInterval = switch.updateAndCheck(IO(sys.error("FAILED"))).await(99.s)
        assert(timeInterval.isDefined)

  "UTC" in:
    check(ZoneId.of("UTC"))

  "Asia/Tbilisi" in :
    check(ZoneId.of("Asia/Tbilisi"))

  "America/Nuuk" in :
    check(ZoneId.of("America/Nuuk"))

  // Mariehamn switched to daylight saving time at 2021-03-28 03:00 //
  // Time zone offset changes from +02:00 to +03:00 //
  private val mariehamnZoneId = ZoneId.of("Europe/Mariehamn")


  "Europe/Mariehamn" - {
    "Daylight saving time" in:
      given zoneId: ZoneId = mariehamnZoneId
      assert(local("2021-03-28T02:59") == ts"2021-03-28T00:59:00Z")
      assert(local("2021-03-28T04:00") == ts"2021-03-28T01:00:00Z")
      assert(local("2021-03-28T04:00") - local("2021-03-28T02:59:59") == 1.s)
      check(zoneId)

    given ZoneId = mariehamnZoneId
    val admissionTimeScheme = AdmissionTimeScheme(Seq(
      WeekdayPeriod(SUNDAY, LocalTime.of(3, 0), 1.h)))

    "Week without a period due to daylight saving time gap" in:
      Dispatcher.parallel[IO].use: dispatcher =>
        given Dispatcher[IO] = dispatcher
        IO:
          val tester = new Tester(admissionTimeScheme)
          tester.check(local("2021-03-21T00:00"),
            admissionStarted = false,
            isAdmitted = false,
            switched = Some(TimeInterval(local("2021-03-21T03:00"), 1.h)))

          tester.check(
            local("2021-03-21T03:00"),
            admissionStarted = true,
            isAdmitted = true)

          // Because the admission is in the dst gap, the next valid local time is returned.
          tester.check(
            local("2021-03-21T04:00"),
            admissionStarted = false,
            isAdmitted = false,
            switched = Some(TimeInterval(local("2021-03-28T04:00"), 1.h)))

          // Admission time 03:00 until 04:00, while clock is skipping this hour.
          // So the admission time ends when it starts.
          // Therefore, the next admission time is selected.
          tester.check(
            local("2021-03-28T04:00"),
            admissionStarted = true,
            isAdmitted = true)

          tester.check(
            local("2021-03-28T04:59"),
            admissionStarted = false,
            isAdmitted = true)

          tester.check(
            local("2021-03-28T05:00"),
            admissionStarted = false,
            isAdmitted = false,
            switched = Some(TimeInterval(local("2021-04-04T03:00"), 1.h)))
  }

  private def check(zoneId: ZoneId): IO[Assertion] =
    given ZoneId = zoneId
    val admissionTimeScheme = AdmissionTimeScheme(Seq(
      WeekdayPeriod(MONDAY, LocalTime.of(8, 0), 2.h),
      WeekdayPeriod(SUNDAY, LocalTime.of(23, 0), 3.h)))  // Until Monday 2:00

    Dispatcher.parallel[IO].use: dispatcher =>
      given Dispatcher[IO] = dispatcher
      IO:
        val tester = new Tester(admissionTimeScheme)
        assert(local("2021-03-22T00:00").toLocalDateTime.getDayOfWeek == MONDAY)

        tester.check(local("2021-03-22T00:00"), // Monday
          switched = Some(TimeInterval(local("2021-03-21T23:00"), 3.h)),
          admissionStarted = false,
          isAdmitted = true)

        tester.check(local("2021-03-22T07:00"),
          switched = Some(TimeInterval(local("2021-03-22T08:00"), 2.h)),
          admissionStarted = false,
          isAdmitted = false)

        tester.check(local("2021-03-22T08:00"),
          admissionStarted = true,
          isAdmitted = true)

        tester.check(local("2021-03-22T09:59"),
          admissionStarted = false,
          isAdmitted = true)

        tester.check(local("2021-03-22T10:00"),
          switched = Some(TimeInterval(local("2021-03-28T23:00"), 3.h)),
          admissionStarted = false,
          isAdmitted = false)

        tester.check(local("2021-03-28T23:00"), // Sunday
          admissionStarted = true,
          isAdmitted = true)


  private final class Tester(admissionTimeScheme: AdmissionTimeScheme)
    (using zone: ZoneId, dispatcher: Dispatcher[IO]):
    import scala.language.unsafeNulls

    private given clock: TestAlarmClock = TestAlarmClock(Timestamp.Epoch)
    private var _switched = null.asInstanceOf[Option[TimeInterval]]
    private val switch = AdmissionTimeSwitch(admissionTimeScheme, 9999.days, zone, jobKey,
      onSwitch = o => IO { _switched = o })
    @volatile private var admissionStarts = 0

    def check(
      now: Timestamp,
      admissionStarted: Boolean,
      isAdmitted: Boolean,
      switched: Option[TimeInterval] = None)
    : Assertion =
      val was = admissionStarts
      val sw = switched.fold(_switched)(Some(_))
      clock := now
      val isAdmitted_ = switch.updateAndCheck:
        IO:
          admissionStarts += 1
      .await(99.s)

      awaitAndAssert(_switched == sw &&
        isAdmitted_.isDefined == isAdmitted &&
        admissionStarts == was + admissionStarted.toInt)
