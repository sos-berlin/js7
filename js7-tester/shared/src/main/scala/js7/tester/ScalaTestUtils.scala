package js7.tester

import org.scalactic.{Prettifier, source}
import org.scalatest.Assertion
import scala.concurrent.blocking
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.{Duration, FiniteDuration, MILLISECONDS, SECONDS}

object ScalaTestUtils extends org.scalatest.Assertions:

  private val DefaultTimeLimit = Duration(10, SECONDS)
  private val DefaultStep = Duration(10, MILLISECONDS)

  def awaitAndAssert(condition: => Boolean)
    (implicit prettifier: Prettifier, pos: source.Position)
  : Assertion =
    awaitAndAssert(DefaultTimeLimit, DefaultStep)(condition)

  def awaitAndAssert(timeLimit: FiniteDuration)
    (condition: => Boolean)
    (implicit prettifier: Prettifier, pos: source.Position)
  : Assertion =
    awaitAndAssert(timeLimit, DefaultStep)(condition)

  def awaitAndAssert(timeLimit: FiniteDuration, step: FiniteDuration = DefaultStep)
    (condition: => Boolean)
    (implicit prettifier: Prettifier, pos: source.Position)
  : Assertion =
    val until = now + timeLimit
    while now < until && !condition do
      blocking:
        Thread.sleep(step.toMillis)
    assert(condition)
