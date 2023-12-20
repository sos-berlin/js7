package js7.base.time

import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.TimeoutWithSteps.deadlineIterator

final class TimeoutWithStepsTest extends OurTestSuite:

  "instantIterator" in:
    implicit val sleeper: MonotonicClock = TestBlockingSleeper()
    import sleeper.deadline

    assert(deadlineIterator(deadline(123.s) + 100.ms, 7.ms, 7.ms).toList ==
      deadline(123.s) + 100.ms :: deadline(123.s) + 107.ms :: Nil)

    assert(deadlineIterator(deadline(123.s) + 100.ms, 7.ms, 3.ms).toList ==
      deadline(123.s) + 100.ms :: deadline(123.s) + 103.ms :: deadline(123.s) + 106.ms :: deadline(123.s) + 107.ms :: Nil)

    assert(deadlineIterator(deadline(123.s) + 100.ms, 3.ms, 7.ms).toList ==
      deadline(123.s) + 100.ms :: deadline(123.s) + 103.ms :: Nil)
