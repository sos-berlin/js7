package js7.tester

import com.softwaremill.diffx
import org.scalatest.Assertion
import org.scalatest.Assertions.succeed
import org.scalatest.exceptions.TestFailedException

object DiffxAssertions
{
  def assertEqual[A: diffx.Diff](a: A, b: A): Assertion = {
    if (a != b) throw new TestFailedException(diffx.compare(a, b).show, 1)
    succeed
  }
}
