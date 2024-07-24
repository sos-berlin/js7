package js7.base.utils

import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*

final class WorryTest extends OurAsyncTestSuite:

  "Default" in :
    assert(Worry() == Worry(
      Seq(3.s, 7.s) ++ Seq.fill(360 - 1)(10.s) ++ Seq(60.s),
      30.s,
      30.s))
