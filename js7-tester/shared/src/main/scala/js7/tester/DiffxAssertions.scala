package js7.tester

import com.softwaremill.diffx

object DiffxAssertions
{
  def assertEqual[A: diffx.Diff](a: A, b: A): Unit =
    if (a != b) {
      throw new AssertionError(diffx.compare(a, b).show)
    }
}
