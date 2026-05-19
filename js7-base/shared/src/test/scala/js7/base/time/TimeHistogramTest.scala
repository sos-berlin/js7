package js7.base.time

import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.TimeHistogramTest.*
import scala.concurrent.duration.*

final class TimeHistogramTest extends OurTestSuite:

  private val logger = Logger[this.type]
  private val periods = Seq(1.s, 10.s, 1.minute, 10.minutes, 1.hour)

  private def counters(histogram: TimeHistogram) =
    histogram.periods.map(histogram.last)

  "test" in:
    val histogram = TimeHistogram(periods)
    Example.foreach: (time, n, sums) =>
      histogram.setTime(time)
      histogram.add(n)
      for (period, i) <- histogram.periods.zipWithIndex do
        val what = s"($time, +$n, ${period.pretty})"
        withClue(s"$what --> "):
          assert(histogram.last(period) == sums(i))
        logger.info(s"$what ✔")

    histogram.setTime(3.h)
    assert(histogram.last(1.h) == 0)

  "test2" in:
    val histogram = TimeHistogram(periods, fractions = 10)
    histogram.setTime(0.s)
    histogram.add(1) // value 1
    histogram.setTime(5.s)
    histogram.add(2) // value 2
    histogram.setTime(9.s)
    histogram.add(3) // value 3
    assert(counters(histogram) == Seq(3, 6, 6, 6, 6))

    // Value 1 is no longer in last second
    histogram.setTime(10.s)
    assert(counters(histogram) == Seq(0, 5, 6, 6, 6))

    histogram.setTime(14.s)
    assert(counters(histogram) == Seq(0, 5, 6, 6, 6))

    // Value 2 is no longer in last 10 seconds
    histogram.setTime(15.s)
    assert(counters(histogram) == Seq(0, 3, 6, 6, 6))

    histogram.setTime(18.s)
    assert(counters(histogram) == Seq(0, 3, 6, 6, 6))

    // Value 3 is no longer in last 10 seconds
    histogram.setTime(19.s)
    assert(counters(histogram) == Seq(0, 0, 6, 6, 6))

    histogram.setTime(1.minute - 1.ns)
    assert(counters(histogram) == Seq(0, 0, 6, 6, 6))

    // Value 3 is no longer in last minute
    histogram.setTime(1.minute)
    assert(counters(histogram) == Seq(0, 0, 3, 6, 6))

    histogram.setTime(1.minute + 5.s)
    assert(counters(histogram) == Seq(0, 0, 3, 6, 6))

    // 1 minute / 10 fractions = 6s
    // Value 3 added 57s ago is no longer in last minute due to fraction duration of 6s
    histogram.setTime(1.minute + 6.s)
    assert(counters(histogram) == Seq(0, 0, 0, 6, 6))

    histogram.setTime(10.minutes - 1.s)
    assert(counters(histogram) == Seq(0, 0, 0, 6, 6))

    histogram.setTime(10.minutes)
    assert(counters(histogram) == Seq(0, 0, 0, 0, 6))

    histogram.setTime(1.h - 1.ms)
    assert(counters(histogram) == Seq(0, 0, 0, 0, 6))

    histogram.setTime(1.h)
    assert(counters(histogram) == Seq(0, 0, 0, 0, 0))


private object TimeHistogramTest:

  val Example = Seq(
    //                              ╭╌╌╌╌╌╌Sum of last╌╌╌╌╌╮
    // time                  n      1s, 10s, 1min, 10min, 1h
    (0.s, 1, Seq(1, 1, 1, 1, 1)),
    (0.s, 2, Seq(3, 3, 3, 3, 3)),
    (1.h, 3, Seq(3, 3, 3, 3, 3)),
    (1.h + 1.minute, 4, Seq(4, 4, 4, 7, 7)),
    (1.h + 1.minute + 10.s, 5, Seq(5, 5, 9, 12, 12)),
    (1.h + 3.minutes, 6, Seq(6, 6, 6, 18, 18)),
    (1.h + 3.minutes + 11.s, 7, Seq(7, 7, 13, 25, 25)),
    (1.h + 3.minutes + 12.s, 8, Seq(8, 15, 21, 33, 33)),
    (1.h + 15.minutes, 9, Seq(9, 9, 9, 9, 42)))
