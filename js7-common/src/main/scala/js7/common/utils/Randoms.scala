package js7.common.utils

import scala.math.*
import scala.util.Random

object Randoms {

  /**
   * Returns a sequence of Int starting at a random value and wrapping around range
   */
  def randomStartInts(range: Iterable[Int]): Iterator[Int] = {
    val r = randomInt(range)
    (r to range.last).iterator ++ (range.head until r).iterator
  }

  def randomInt(r: Iterable[Int]): Int =
    r.head + abs(Random.nextInt()) % (r.last - r.head + 1)
}
