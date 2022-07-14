package js7.common.akkahttp.web.session

import monix.execution.schedulers.TestScheduler
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration.*

/**
  * @author Joacim Zschimmer
  */
final class HasTimeoutTest extends AnyFreeSpec
{
  private val t = new HasTimeout {}

  "test" in {
    implicit val scheduler = TestScheduler()
    assert(t.isEternal)
    assert(t.isAlive)

    t.touch(1.hour)
    assert(!t.isEternal)
    assert(t.isAlive)
    scheduler.tick(1.minute)
    assert(t.isAlive)

    scheduler.tick(1.hour)
    assert(!t.isAlive)
  }
}
