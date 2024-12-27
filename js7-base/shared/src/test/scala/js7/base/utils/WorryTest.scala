package js7.base.utils

import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.base.utils.Worry.adaptDurationsToMinimum

final class WorryTest extends OurAsyncTestSuite:

  "Default" in :
    assert(Worry() == Worry(
      List(3.s, 7.s) ::: List.fill(360 - 1)(10.s) ::: 60.s :: Nil,
      30.s,
      30.s))

  "adaptDurationsToMinimum" in:
    val worryDurations = List(3.s, 7.s, 10.s, 40.s, 60.s)

    assert(adaptDurationsToMinimum(-1.s, worryDurations) == Seq(-1.s, 4.s, 7.s, 10.s, 40.s, 60.s))
    assert(adaptDurationsToMinimum(0.s, worryDurations) == Seq(0.s, 3.s, 7.s, 10.s, 40.s, 60.s))

    assert(adaptDurationsToMinimum(1.s, worryDurations) == Seq(1.s, 2.s, 7.s, 10.s, 40.s, 60.s))
    assert(adaptDurationsToMinimum(3.s, worryDurations) == Seq(3.s, 7.s, 10.s, 40.s, 60.s))
    assert(adaptDurationsToMinimum(4.s, worryDurations) == Seq(4.s, 6.s, 10.s, 40.s, 60.s))
    assert(adaptDurationsToMinimum(9.s, worryDurations) == Seq(9.s, 11.s, 40.s, 60.s))
    assert(adaptDurationsToMinimum(10.s, worryDurations) == Seq(10.s, 10.s, 40.s, 60.s))
    assert(adaptDurationsToMinimum(11.s, worryDurations) == Seq(11.s, 49.s, 60.s))
