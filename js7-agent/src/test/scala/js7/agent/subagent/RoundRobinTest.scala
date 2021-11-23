package js7.agent.subagent

import org.scalatest.freespec.AnyFreeSpec

final class RoundRobinTest extends AnyFreeSpec
{
  "next" in {
    val roundRobin = new RoundRobin
    assert(roundRobin.next(4) == 0)
    assert(roundRobin.next(4) == 1)
    assert(roundRobin.next(4) == 2)
    assert(roundRobin.next(4) == 3)
    assert(roundRobin.next(4) == 0)
    assert(roundRobin.next(4) == 1)
    assert(roundRobin.next(4) == 2)

    assert(roundRobin.next(2) == 0)
    assert(roundRobin.next(2) == 1)
    assert(roundRobin.next(2) == 0)
    assert(roundRobin.next(2) == 1)

    assert(roundRobin.next(4) == 2)

    assert(roundRobin.next(1) == 0)
    assert(roundRobin.next(1) == 0)

    // 0 is an invalid argument, but yields 0 (for now)
    assert(roundRobin.next(0) == 0)
  }
}
