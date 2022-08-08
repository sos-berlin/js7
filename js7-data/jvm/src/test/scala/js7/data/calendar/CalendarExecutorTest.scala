package js7.data.calendar

import java.time.ZoneId
import js7.base.problem.Problem
import js7.base.time.JavaTimestamp.local
import js7.base.time.ScalaTime.*
import js7.base.time.{TimeInterval, Timestamp, Timezone}
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.order.OrderId
import org.scalatest.freespec.AnyFreeSpec

final class CalendarExecutorTest extends AnyFreeSpec
{
  private implicit val zoneId: ZoneId = ZoneId.of("Europe/Mariehamn")

  private val calendar = Calendar.daily(
    CalendarPath("CALENDAR"),
    Timezone(zoneId.toString),
    dateOffset = 6.h)

  "Period is a day" - {
    val calendarExecutor = CalendarExecutor.checked(calendar).orThrow
    import calendarExecutor.orderIdToTimeInterval

    "Invalid" in {
      assert(orderIdToTimeInterval(OrderId("x")) ==
        Left(Problem("Order:x does match the pattern defined in Calendar:CALENDAR")))

      assert(orderIdToTimeInterval(OrderId("PREFIX#2021-10-01#")) ==
        Left(Problem("Order:PREFIX#2021-10-01# does match the pattern defined in Calendar:CALENDAR")))

      assert(orderIdToTimeInterval(OrderId("#2021-10-1#")) ==
        Left(Problem("java.time.format.DateTimeParseException: Text '2021-10-1' could not be parsed at index 8")))

      assert(orderIdToTimeInterval(OrderId("#2021-10-01#")).isRight)
      assert(orderIdToTimeInterval(OrderId("#2021-10-01#SUFFIX")).isRight)
    }

    "Summer" in {
      assert(orderIdToTimeInterval(OrderId("#2021-10-01#")) ==
        Right(TimeInterval(local("2021-10-01T06:00"), 24.h)))
    }

    "While changing to daylight saving time" in {
      assert(orderIdToTimeInterval(OrderId("#2021-03-27#")) ==
        Right(TimeInterval(local("2021-03-27T06:00"), 24.h - 1.h)))
    }

    "While leaving to daylight saving time" in {
      assert(orderIdToTimeInterval(OrderId("#2021-10-30#")) ==
        Right(TimeInterval(local("2021-10-30T06:00"), 24.h + 1.h)))
    }
  }

  "Period is a week (experimental)" in {
    val weekCalendar = Calendar
      .checked(
        CalendarPath("CALENDAR"),
        Timezone(zoneId.toString),
        dateOffset = 6.h,
        orderIdToDatePattern = Calendar.orderIdToDatePatternDefault,
        periodDatePattern = "YYYY-'W'w")
      .orThrow

    def f(orderId: OrderId): Timestamp = {
      val checkedTimeInterval = for {
        executor <- CalendarExecutor.checked(weekCalendar)
        timeInterval <- executor.orderIdToTimeInterval(orderId)
      } yield timeInterval
      assert(checkedTimeInterval.orThrow.duration == 7 * 24.h)
      checkedTimeInterval.orThrow.start
    }

    assert(f(OrderId("#2011-W1#")) == local("2010-12-27T06:00"))  // 2016-01-03 ?
    assert(f(OrderId("#2015-W1#")) == local("2014-12-29T06:00"))
    assert(f(OrderId("#2016-W1#")) == local("2015-12-28T06:00"))  // 2016-01-04 ?
    assert(f(OrderId("#2017-W1#")) == local("2017-01-02T06:00"))
    assert(f(OrderId("#2018-W1#")) == local("2018-01-01T06:00"))
    assert(f(OrderId("#2019-W1#")) == local("2018-12-31T06:00"))
    assert(f(OrderId("#2020-W1#")) == local("2019-12-30T06:00"))
    assert(f(OrderId("#2021-W1#")) == local("2020-12-28T06:00"))  // 2021-01-04 ?

    pending // TODO Week calendar dates are not as expected
  }
}
