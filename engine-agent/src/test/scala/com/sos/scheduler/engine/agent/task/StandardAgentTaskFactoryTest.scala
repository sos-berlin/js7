package com.sos.scheduler.engine.agent.task

import com.sos.scheduler.engine.agent.task.StandardAgentTaskFactory.newAgentTaskIdGenerator
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class StandardAgentTaskFactoryTest extends FreeSpec {

  "newAgentTaskIdGenerator" in {
    val next = newAgentTaskIdGenerator().next _
    for (i ← 1 to 10000) {
      val id = next()
      assert(id.value >= 1)
      assert(id.index == i)
    }
  }

  "newAgentTaskIdGenerator overflow" in {
    val next = newAgentTaskIdGenerator(Int.MaxValue - 100).next _
    for (_ ← 1 to 10000) {
      assert(next().value >= 1)
    }
  }

  "newAgentTaskIdGenerator overflow 2" in {
    val ids = (newAgentTaskIdGenerator(Int.MaxValue - 2) take 4).toList
    ids map { _.index } shouldEqual List(Int.MaxValue - 2, Int.MaxValue - 1, 1, 2)
  }
}
