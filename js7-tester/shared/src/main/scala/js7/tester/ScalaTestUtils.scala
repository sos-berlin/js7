package js7.tester

import org.scalactic.{Prettifier, source}
import org.scalatest.Assertion
import scala.concurrent.blocking
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.{Duration, FiniteDuration, MILLISECONDS, SECONDS}

object ScalaTestUtils extends org.scalatest.Assertions:

  private val DefaultTimeLimit = Duration(10, SECONDS)
  private val DefaultStep = Duration(10, MILLISECONDS)

  inline def awaitAndAssert(inline condition: => Boolean)
    (implicit prettifier: Prettifier, pos: source.Position)
  : Assertion =
    awaitAndAssert(DefaultTimeLimit, DefaultStep)(condition)

  inline def awaitAndAssert(timeLimit: FiniteDuration)
    (inline condition: => Boolean)
    (implicit prettifier: Prettifier, pos: source.Position)
  : Assertion =
    awaitAndAssert(timeLimit, DefaultStep)(condition)

  inline def awaitAndAssert(timeLimit: FiniteDuration, step: FiniteDuration = DefaultStep)
    (inline condition: => Boolean)
    (implicit prettifier: Prettifier, pos: source.Position)
  : Assertion =
    val until = now + timeLimit
    while now < until && !condition do
      blocking:
        Thread.sleep(step.toMillis)
    assert(condition)
