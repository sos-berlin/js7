package js7.base.time

import com.typesafe.config.ConfigFactory
import js7.base.problem.Problem
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax._
import monix.execution.schedulers.TestScheduler
import org.scalatest.freespec.AnyFreeSpec

final class DelayIteratorTest extends AnyFreeSpec
{
  private implicit val scheduler = TestScheduler()

  "Wrong type" in {
    val config = ConfigFactory.parseString("""durations = true""")
    assert(DelayIterator.fromConfig(config, "durations") == Left(Problem(
      "com.typesafe.config.ConfigException$WrongType: String: 1: durations has type BOOLEAN rather than LIST")))
  }

  "Empty sequence" in {
    val config = ConfigFactory.parseString("""durations = []""")
    assert(DelayIterator.fromConfig(config, "durations") == Left(Problem(
      "Setting 'durations' must not be empty")))
  }

  "Simple test" in {
    val config = ConfigFactory.parseString("""durations = [3s, 7s, 10s]""")
    assert(DelayIterator.fromConfig(config, "durations").orThrow.take(5).toSeq ==
      Seq(3.s, 10.s, 20.s, 30.s, 40.s))
  }

  "After time elapsed" in {
    val config = ConfigFactory.parseString("""durations = [3s, 7s, 10s]""")
    val it = DelayIterator.fromConfig(config, "durations").orThrow
    assert(it.next() == 3.s)

    scheduler.tick(4.s)
    assert(it.next() == 6.s)

    scheduler.tick(6.s + 4.s)
    assert(it.next() == 6.s)

    scheduler.tick(6.s + 11.s)
    assert(it.next() == 0.s)
    assert(it.next() == 9.s)
  }
}
