package js7.data.execution.workflow.instructions

import cats.effect.unsafe.IORuntime
import java.time.DayOfWeek.{MONDAY, SUNDAY}
import java.time.ZoneOffset.UTC
import java.time.{LocalTime, ZoneId}
import js7.base.test.OurTestSuite
import js7.base.time.JavaTimestamp.local
import js7.base.time.JavaTimestamp.specific.RichJavaTimestamp
import js7.base.time.ScalaTime.*
import js7.base.time.TimestampForTests.ts
import js7.base.time.{AdmissionTimeScheme, TestAlarmClock, TimeInterval, Timestamp, WeekdayPeriod}
import js7.base.utils.ScalaUtils.syntax.*
import scala.concurrent.duration.*

final class ExecuteAdmissionTimeSwitchTest extends OurTestSuite:

  private given IORuntime = ioRuntime

  "AdmissionTimeScheme.always" in:
    val now = ts"2021-01-01T12:00:00Z"
    implicit val alarmClock: TestAlarmClock = TestAlarmClock(now)
    val switch = new ExecuteAdmissionTimeSwitch(AdmissionTimeScheme.always, 9999.days, UTC, _ => ())

    switch.updateAndCheck(sys.error("FAILED"))
    assert(switch.nextTime == None)

    var isEnterable = switch.updateAndCheck(sys.error("FAIsLED"))
    assert(isEnterable)
    assert(switch.nextTime == None)

    alarmClock += 99*365*24.h
    isEnterable = switch.updateAndCheck(sys.error("FAILED"))
    assert(isEnterable)

  "UTC" in:
    check(ZoneId.of("UTC"))

  // --> Mariehamn switched to daylight saving time at 2021-03-28 03:00 <-- !!!
  // Time zone offset changes from +02:00 to +03:00
  private val mariehamnZoneId = ZoneId.of("Europe/Mariehamn")


  "Europe/Mariehamn" - {
    "Daylight saving time" in:
      implicit val zone = mariehamnZoneId
      assert(local("2021-03-28T02:59") == ts"2021-03-28T00:59:00Z")
      assert(local("2021-03-28T04:00") == ts"2021-03-28T01:00:00Z")
      assert(local("2021-03-28T04:00") - local("2021-03-28T02:59:59") == 1.s)

      check(zone)

    implicit val zone = mariehamnZoneId
    val admissionTimeScheme = AdmissionTimeScheme(Seq(
      WeekdayPeriod(SUNDAY, LocalTime.of(3, 0), 1.h)))

    "Week without a period due to daylight saving time gap" in:
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

  private def check(zone: ZoneId): Unit =
    implicit val implicitZoneId = zone
    val admissionTimeScheme = AdmissionTimeScheme(Seq(
      WeekdayPeriod(MONDAY, LocalTime.of(8, 0), 2.h),
      WeekdayPeriod(SUNDAY, LocalTime.of(23, 0), 3.h)))  // Until Monday 2:00

    val tester = new Tester(admissionTimeScheme)
    assert(local("2021-03-22T00:00").toLocalDateTime(using zone).getDayOfWeek == MONDAY)

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


  private final class Tester(admissionTimeScheme: AdmissionTimeScheme)(using zone: ZoneId):
    import scala.language.unsafeNulls

    private implicit val clock: TestAlarmClock = TestAlarmClock(Timestamp.Epoch)
    private var _switched: Option[TimeInterval] = null
    private val switch = new ExecuteAdmissionTimeSwitch(admissionTimeScheme, 9999.days, zone,
      onSwitch = _switched = _)
    private var admissionStarts = 0

    def check(
      now: Timestamp,
      admissionStarted: Boolean,
      isAdmitted: Boolean,
      switched: Option[TimeInterval] = None)
    : Unit =
      val was = admissionStarts
      val sw = switched.fold(_switched)(Some(_))
      clock := now
      val isAdmitted_ = switch.updateAndCheck:
        admissionStarts += 1

      assert(_switched == sw &&
        isAdmitted_ == isAdmitted &&
        admissionStarts == was + admissionStarted.toInt)
