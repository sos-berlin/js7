package com.sos.scheduler.engine.data.agent

import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class RemoteTaskIdTest extends FreeSpec {

  "newGenerator" in {
    val next = RemoteTaskId.newGenerator().next _
    for (i ← 1 to 10000) {
      val id = next()
      assert(id.value >= 1)
      assert(id.index == i)
    }
  }

  "newGenerator overflow" in {
    val next = RemoteTaskId.newGenerator(Int.MaxValue - 100).next _
    for (_ ← 1 to 10000) {
      assert(next().value >= 1)
    }
  }

  "newGenerator overflow 2" in {
    val ids = (RemoteTaskId.newGenerator(Int.MaxValue - 2) take 4).toList
    ids map { _.index } shouldEqual List(Int.MaxValue - 2, Int.MaxValue - 1, 1, 2)
  }
}
