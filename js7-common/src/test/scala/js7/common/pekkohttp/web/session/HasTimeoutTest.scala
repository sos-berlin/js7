package js7.common.pekkohttp.web.session

import cats.effect.IO
import cats.effect.testkit.TestControl
import js7.base.catsutils.CatsDeadline
import js7.base.test.OurAsyncTestSuite
import scala.concurrent.duration.*

/**
  * @author Joacim Zschimmer
  */
final class HasTimeoutTest extends OurAsyncTestSuite:

  "test" in:
    TestControl.executeEmbed:
      val t = new HasTimeout {}
      assert(t.isEternal)
      for
        _ <- t.isAlive.map(o => assert(o))
        now <- CatsDeadline.now
        timeoutFiber <- IO.unit.start
        _ <- t.timeoutAt(timeoutAt = now + 1.hour, timeoutFiber)
        _ = assert(!t.isEternal)
        _ <- t.isAlive.map(o => assert(o))
        _ <- IO.sleep(1.minute)
        _ <- t.isAlive.map(o => assert(o))
        _ <- IO.sleep(1.hour)
        _ <- t.isAlive.map(o => assert(!o))
      yield succeed
