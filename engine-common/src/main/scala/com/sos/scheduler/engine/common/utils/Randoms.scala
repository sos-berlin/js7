package com.sos.scheduler.engine.common.utils

import scala.math._
import scala.util.Random

object Randoms {

  def randomInts(range: Iterable[Int]): Iterator[Int] = {
    val r = randomInt(range)
    (r to range.last).toIterator ++ (range.head until r).toIterator
  }

  def randomInt(r: Iterable[Int]): Int =
    r.head + abs(Random.nextInt()) % (r.last - r.head + 1)
}
