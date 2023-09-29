package js7.subagent.director

import js7.base.test.OurTestSuite

final class FixedPriorityTest extends OurTestSuite:
  "next, same priority" in:
    def isEquivalent(i: Int, j: Int) = true

    val fixedPriority = new FixedPriority

    assert(fixedPriority.next(4, isEquivalent) == 0)
    assert(fixedPriority.next(4, isEquivalent) == 1)
    assert(fixedPriority.next(4, isEquivalent) == 2)
    assert(fixedPriority.next(4, isEquivalent) == 3)
    assert(fixedPriority.next(4, isEquivalent) == 0)
    assert(fixedPriority.next(4, isEquivalent) == 1)
    assert(fixedPriority.next(4, isEquivalent) == 2)

    assert(fixedPriority.next(2, isEquivalent) == 0)
    assert(fixedPriority.next(2, isEquivalent) == 1)
    assert(fixedPriority.next(2, isEquivalent) == 0)
    assert(fixedPriority.next(2, isEquivalent) == 1)

    assert(fixedPriority.next(4, isEquivalent) == 0)

    assert(fixedPriority.next(1, isEquivalent) == 0)
    assert(fixedPriority.next(1, isEquivalent) == 0)

    // 0 is an invalid argument, but yields 0 (for now)
    assert(fixedPriority.next(0, isEquivalent) == 0)

  "next, different priorities" in:
    val priorities = Vector(1, 1, 1, 0)
    def isEquivalent(i: Int, j: Int) = priorities(i) == priorities(j)

    val fixedPriority = new FixedPriority
    val n = priorities.size
    assert(fixedPriority.next(n, isEquivalent) == 0)
    assert(fixedPriority.next(n, isEquivalent) == 1)
    assert(fixedPriority.next(n, isEquivalent) == 2)
    assert(fixedPriority.next(n, isEquivalent) == 0)
    assert(fixedPriority.next(n, isEquivalent) == 1)
    assert(fixedPriority.next(n, isEquivalent) == 2)
    assert(fixedPriority.next(n, isEquivalent) == 0)
