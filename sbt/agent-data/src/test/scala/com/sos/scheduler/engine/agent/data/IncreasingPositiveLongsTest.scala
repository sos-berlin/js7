package com.sos.scheduler.engine.agent.data

import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class IncreasingPositiveLongsTest extends FreeSpec {

  "Only positives" in {
    val iterator = new IncreasingPositiveLongs()
    for (_ ← 1 to 10000) assert(iterator.next() >= 1)
  }

  "overflow" in {
    val start = Long.MaxValue - 100
    val iterator = new IncreasingPositiveLongs(start = start)
    for (_ ← 0 to 10000) assert(iterator.next() >= 1)
  }

  "overflow 2" in {
    val list = (new IncreasingPositiveLongs(start = Long.MaxValue - 2) take 5).toList
    list shouldEqual List(Long.MaxValue - 2, Long.MaxValue - 1, Long.MaxValue, 1, 2)
  }
}
