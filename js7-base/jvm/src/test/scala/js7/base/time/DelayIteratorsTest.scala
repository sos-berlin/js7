package js7.base.time

import cats.effect.IO
import com.typesafe.config.ConfigFactory
import js7.base.problem.Problem
import js7.base.test.{OurAsyncTestSuite, OurTestControl}
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*

final class DelayIteratorsTest extends OurAsyncTestSuite:

  "Wrong type" in:
    OurTestControl.executeEmbed:
      IO:
        val config = ConfigFactory.parseString("""durations = true""")
        assert(DelayIterators.fromConfig(config, "durations") == Left(Problem(
          "com.typesafe.config.ConfigException$WrongType: String: 1: durations has type BOOLEAN rather than LIST")))

  "Empty sequence" in:
    OurTestControl.executeEmbed:
      IO:
        val config = ConfigFactory.parseString("""durations = []""")
        assert(DelayIterators.fromConfig(config, "durations") == Left(Problem(
          "Setting 'durations' must not be empty")))

  "Simple test" in:
    OurTestControl.executeEmbed:
      IO:
        val config = ConfigFactory.parseString("""durations = [3s, 7s, 10s]""")
        assert(DelayIterators.fromConfig(config, "durations").orThrow.take(5).toSeq ==
          Seq(3.s, 10.s, 20.s, 30.s, 40.s))

  "After time elapsed" in:
    val config = ConfigFactory.parseString("""durations = [3s, 7s, 10s]""")
    OurTestControl.executeEmbed:
      val it = DelayIterators.fromConfig(config, "durations").orThrow
      assert(it.next() == 3.s)

      for
        _ <- IO.sleep(4.s)
        _ = assert(it.next() == 6.s)

        _ <- IO.sleep(6.s + 4.s)
        _ = assert(it.next() == 6.s)

        _ <- IO.sleep(6.s + 11.s)
      yield
        assert(it.next() == 0.s)
        assert(it.next() == 9.s)
