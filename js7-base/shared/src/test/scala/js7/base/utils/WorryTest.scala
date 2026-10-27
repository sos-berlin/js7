package js7.base.utils

import cats.effect.testkit.TestControl
import cats.effect.{IO, Ref}
import js7.base.log.Logger.syntax.*
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.base.utils.Worry.adaptDurationsToMinimum
import scala.annotation.nowarn
import scala.concurrent.duration.{Duration, FiniteDuration}

@nowarn("msg=Illegal literal")
final class WorryTest extends OurAsyncTestSuite:

  private lazy val logger = Logger[this.type]

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

  "whenItTakesLonger" - {
    val worry = Worry(durations = List(1.s, 3.s, 6.s, 10.s), infoLevel = 1.h, orangeLevel = 1.h)

    /** Runs `use` in the resource and returns the result of the use together with the calls
      * of onDelayedOrCompleted: None while waiting, Some(Outcome) when the use has ended. */
    def run(worry: Worry, timeout: Duration = Duration.Inf)(use: IO[Unit])
    : IO[(Either[Throwable, Unit], Vector[FiniteDuration])] =
      TestControl.executeEmbed:
        for
          events <- Ref.of[IO, Vector[FiniteDuration]](Vector.empty)
          result <-
            worry.whenItTakesLonger: elapsed =>
              events.update(_ :+ elapsed)
            .surround:
              logger.debugIO("USE"):
                use
            .timeout(timeout)
            .attempt
          recorded <- events.get
        yield
          (result, recorded)

    "Logs while in use, then logs completion when released" in:
      run(worry):
        IO.sleep(22.s)
      .map: (result, events) =>
        assert:
          result == Right(()) && events == List(1.s, 4.s, 10.s, 20.s)

    "Logs nothing after release" in:
      run(worry):
        IO.sleep(2.s)
      .flatMap: (_, events) =>
        IO.sleep(100.ms).as:
          assert(events == List(1.s))

    "Failing use" in:
      run(worry):
        IO.sleep(5.s) *>
          IO.raiseError(IllegalStateException("TEST"))
      .map: (result, events) =>
        assert(result.left.exists(_.isInstanceOf[IllegalStateException]))
        assert(events == List(1.s, 4.s))

    "Timeout" in:
      run(worry, timeout = 9.s):
        IO.never
      .map: (result, events) =>
        assert(result.left.exists(_.isInstanceOf[java.util.concurrent.TimeoutException]))
        assert(events == List(1.s, 4.s))

    "Nothing is logged if the use is quick" in:
      run(Worry.Default):
        IO.unit
      .map: (result, events) =>
        assert(result == Right(()) && events == Nil)
  }

  "logWhenItTakesLonger logs messages" in:
    val worry = Worry(durations = List(10.ms), infoLevel = 1.h, orangeLevel = 1.h)
    TestControl.executeEmbed:
      worry.logWhenItTakesLonger("TEST")
        .surround:
          IO.sleep(100.ms)
        .timeout(9.s)
        .as(succeed)
