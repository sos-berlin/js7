package com.sos.scheduler.engine.agent.process

import com.sos.scheduler.engine.agent.process.StandardAgentProcessFactory.newAgentProcessIdGenerator
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class StandardAgentProcessFactoryTest extends FreeSpec {

  "newAgentProcessIdGenerator" in {
    val next = newAgentProcessIdGenerator().next _
    for (i ← 1 to 10000) {
      val id = next()
      assert(id.value >= 1)
      assert(id.index == i)
    }
  }

  "newAgentProcessIdGenerator overflow" in {
    val next = newAgentProcessIdGenerator(Int.MaxValue - 100).next _
    for (_ ← 1 to 10000) {
      assert(next().value >= 1)
    }
  }

  "newAgentProcessIdGenerator overflow 2" in {
    val ids = (newAgentProcessIdGenerator(Int.MaxValue - 2) take 4).toList
    ids map { _.index } shouldEqual List(Int.MaxValue - 2, Int.MaxValue - 1, 1, 2)
  }
}
