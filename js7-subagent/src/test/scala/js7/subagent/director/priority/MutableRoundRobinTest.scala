package js7.subagent.director.priority

import js7.base.test.OurAsyncTestSuite

final class MutableRoundRobinTest extends OurAsyncTestSuite:

  "nextIndex" in:
    val roundRobin = MutableRoundRobin()
    assert(roundRobin.next(3) == 0)
    assert(roundRobin.next(3) == 1)
    assert(roundRobin.next(3) == 2)
    assert(roundRobin.next(3) == 0)
    assert(roundRobin.next(3) == 1)
    assert(roundRobin.next(4) == 2)
    assert(roundRobin.next(4) == 3)
    assert(roundRobin.next(4) == 0)
    assert(roundRobin.next(2) == 1)
    assert(roundRobin.next(1) == 0)
    assert(roundRobin.next(1) == 0)
    assert(roundRobin.next(1) == 0)
    assert(roundRobin.next(0) == 0)
    assert(roundRobin.next(0) == 0)
    assert(roundRobin.next(0) == 0)

  "reset" in:
    val roundRobin = MutableRoundRobin()
    assert(roundRobin.next(3) == 0)
    assert(roundRobin.next(3) == 1)
    roundRobin.reset()
    assert(roundRobin.next(3) == 0)
    assert(roundRobin.next(3) == 1)
    assert(roundRobin.next(3) == 2)
