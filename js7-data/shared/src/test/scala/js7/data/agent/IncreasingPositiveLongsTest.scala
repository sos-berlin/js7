package js7.data.agent

import js7.base.test.Test
import org.scalatest.matchers.should.Matchers.*

/**
 * @author Joacim Zschimmer
 */
final class IncreasingPositiveLongsTest extends Test
{
  "Only positives" in {
    val iterator = new IncreasingPositiveLongs()
    for (_ <- 1 to 10000) assert(iterator.next() >= 1)
  }

  "overflow" in {
    val start = Long.MaxValue - 100
    val iterator = new IncreasingPositiveLongs(start = start)
    for (_ <- 0 to 10000) assert(iterator.next() >= 1)
  }

  "overflow 2" in {
    val list = (new IncreasingPositiveLongs(start = Long.MaxValue - 2) take 5).toList
    list shouldEqual List(Long.MaxValue - 2, Long.MaxValue - 1, Long.MaxValue, 1, 2)
  }
}
