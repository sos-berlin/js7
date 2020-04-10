package com.sos.jobscheduler.agent.data

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers
import org.scalatest.matchers.should.Matchers._

/**
 * @author Joacim Zschimmer
 */
final class AgentTaskIdTest extends AnyFreeSpec {

  "toString" in {
    assert(AgentTaskId(123, 789).toString == "AgentTaskId(123-789)")
    assert(AgentTaskId(-123, 789).toString == "AgentTaskId(-123-789)")
  }

  "apply(string)" in {
    assert(AgentTaskId("123-789").string == "123-789")
    assert(AgentTaskId("-123-789").string == "-123-789")
    assert(AgentTaskId("123000000789").string == "123000000789")
    AgentTaskId("allowed-characters-ABCDEFGHIJKLMNOPQRSTUVWXYZ-abcdefghijklmnopqrstuvwxyz-012356789_.")
    intercept[IllegalArgumentException] { AgentTaskId("") }
    intercept[IllegalArgumentException] { AgentTaskId("with space") }
    intercept[IllegalArgumentException] { AgentTaskId("with-$variable") }
  }

  "AgentTaskIdGenerator" in {
    val generator = AgentTaskId.newGenerator()
    for (i <- 1 to 10000) {
      val id = generator.next()
      assert(id.index == i)
    }
  }

  "AgentTaskIdGenerator overflow" in {
    val ids = (AgentTaskId.newGenerator(start = Int.MaxValue - 2) take 5).toList
    ids map { _.index } shouldEqual List(Int.MaxValue - 2, Int.MaxValue - 1, Int.MaxValue, 1, 2)
  }
}
