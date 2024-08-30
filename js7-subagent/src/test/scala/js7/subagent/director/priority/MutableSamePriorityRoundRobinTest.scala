package js7.subagent.director.priority

import js7.base.test.OurTestSuite

final class MutableSamePriorityRoundRobinTest extends OurTestSuite:

  "next, same priority, always round-robin" in:
    var n = 4

    // Throws if not (i < n and j < n)
    def isEquivalent(i: Int, j: Int) =
      require(i >= 0 && i < n && j >= 0 && j < n)
      true

    val fixedPriority = new MutableSamePriorityRoundRobin

    n = 4
    assert(fixedPriority.next(n, isEquivalent) == 0)
    assert(fixedPriority.next(n, isEquivalent) == 1)
    assert(fixedPriority.next(n, isEquivalent) == 2)
    assert(fixedPriority.next(n, isEquivalent) == 3)
    assert(fixedPriority.next(n, isEquivalent) == 0)
    assert(fixedPriority.next(n, isEquivalent) == 1)
    assert(fixedPriority.next(n, isEquivalent) == 2)

    n = 2
    assert(fixedPriority.nextIndex == 3) // 3 > n
    assert(fixedPriority.next(n, isEquivalent) == 0)
    assert(fixedPriority.next(n, isEquivalent) == 1)
    assert(fixedPriority.next(n, isEquivalent) == 0)
    assert(fixedPriority.next(n, isEquivalent) == 1)

    n = 4
    assert(fixedPriority.next(n, isEquivalent) == 0)

    n = 1
    assert(fixedPriority.next(n, isEquivalent) == 0)
    assert(fixedPriority.next(n, isEquivalent) == 0)

    // 0 is an invalid argument, but yields 0 (for now)
    n = 0
    assert(fixedPriority.next(n, isEquivalent) == 0)

  "next, different priorities" in:
    val priorities = Vector(8, 8, 8, 7)
    def isEquivalent(i: Int, j: Int) = priorities(i) == priorities(j)

    val fixedPriority = new MutableSamePriorityRoundRobin
    val n = priorities.size
    assert(fixedPriority.next(n, isEquivalent) == 0) // initial
    assert(fixedPriority.next(n, isEquivalent) == 1) // round-robin
    assert(fixedPriority.next(n, isEquivalent) == 2) // round-robin

    assert(fixedPriority.next(n, isEquivalent) == 0) // priority changed
    assert(fixedPriority.next(n, isEquivalent) == 1) // round-robin
    assert(fixedPriority.next(n, isEquivalent) == 2) // round-robin

    assert(fixedPriority.next(n, isEquivalent) == 0) // priority changed
