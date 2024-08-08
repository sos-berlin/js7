package js7.tester

import org.scalatest.Assertion
import org.scalatest.Assertions.assert

object DiffxAssertions:

  def assertEqual[A](a: A, b: A): Assertion =
    assert(a == b)
