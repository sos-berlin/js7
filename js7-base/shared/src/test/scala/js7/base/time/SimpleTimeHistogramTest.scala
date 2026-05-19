package js7.base.time

import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.SimpleTimeHistogramTest.*
import js7.base.time.SimpleTimeHistogram
import scala.concurrent.duration.*

final class SimpleTimeHistogramTest extends OurTestSuite:

  private val logger = Logger[this.type]

  "test" in:
    val histogram = SimpleTimeHistogram(period = 1.s, 3600)
    val periods = Seq(1.s, 10.s, 1.minute, 10.minutes, 1.hour)
    Example.foreach: (time, n, sums) =>
      histogram.setTime(time)
      histogram.add(n)
      for (period, i) <- periods.zipWithIndex do
        val what = s"($time, +$n, ${period.pretty})"
        withClue(s"$what --> "):
          assert(histogram.last((period.toNanos / histogram.period.toNanos).toInt) == sums(i))
        logger.info(s"$what ✔")

    assert(histogram.last(15*60 - 3*60 - 12) == 9)
    assert(histogram.last(15*60 - 3*60) == 7 + 8 + 9)
    assert(histogram.last(15*60) == 4 + 5 + 6 + 7 + 8 + 9)
    assert(histogram.last(15*60 + 1) == 3 + 4 + 5 + 6 + 7 + 8 + 9)
    assert(histogram.last(Int.MaxValue) == 3 + 4 + 5 + 6 + 7 + 8 + 9)

    histogram.setTime(3.h)
    assert(histogram.last(Int.MaxValue) == 0)


private object SimpleTimeHistogramTest:
  val Example = Seq(
    //                              ╭╌╌╌╌╌╌Sum of last╌╌╌╌╌╮
    // time                  n      1s, 10s, 1min, 10min, 1h
    (0.s                   , 1, Seq( 1,   1,    1,     1,  1)),
    (0.s                   , 2, Seq( 3,   3,    3,     3,  3)),
    (1.h                   , 3, Seq( 3,   3,    3,     3,  3)),
    (1.h + 1.minute        , 4, Seq( 4,   4,    4,     7,  7)),
    (1.h + 1.minute + 10.s , 5, Seq( 5,   5,    9,    12, 12)),
    (1.h + 3.minutes       , 6, Seq( 6,   6,    6,    18, 18)),
    (1.h + 3.minutes + 11.s, 7, Seq( 7,   7,   13,    25, 25)),
    (1.h + 3.minutes + 12.s, 8, Seq( 8,  15,   21,    33, 33)),
    (1.h + 15.minutes      , 9, Seq( 9,   9,    9,     9, 42)))
