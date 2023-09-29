package js7.tester

//diffx import com.softwaremill.diffx
import org.scalatest.Assertion
import org.scalatest.Assertions.assert

object DiffxAssertions
{
  def assertEqual[A/*: diffx.Diff*/](a: A, b: A): Assertion = {
    assert(a == b)
    //diffx if (a != b)
    //diffx   throw new TestFailedException(diffx.compare(a, b).show(), 1)
    //diffx succeed
  }
}
