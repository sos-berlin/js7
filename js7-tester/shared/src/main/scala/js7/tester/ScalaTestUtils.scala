package js7.tester

import org.scalactic.{Prettifier, source}
import org.scalatest.Assertion
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.{Duration, FiniteDuration, SECONDS}

object ScalaTestUtils extends org.scalatest.Assertions
{
  def awaitAndAssert(condition: => Boolean)
    (implicit prettifier: Prettifier, pos: source.Position)
  : Assertion =
    awaitAndAssert(Duration(10, SECONDS))(condition)

  def awaitAndAssert(timeLimit: FiniteDuration)(condition: => Boolean)
    (implicit prettifier: Prettifier, pos: source.Position)
  : Assertion = {
    val until = now + timeLimit
    while (now < until && !condition) Thread.sleep(10)
    assert(condition)
  }
}
