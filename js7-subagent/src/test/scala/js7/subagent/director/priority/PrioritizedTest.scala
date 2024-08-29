package js7.subagent.director.priority

import js7.base.test.OurTestSuite
import js7.subagent.director.priority.Prioritized.prioritySort
import js7.subagent.director.priority.PrioritizedTest.*

final class PrioritizedTest extends OurTestSuite:

  "prioritySort" in:
    assert(prioritySort(Vector.empty[String])(_ => 0).isEmpty)
    assert(prioritySort(Vector("1-1", "10-2", "B-1", "B-2", "A-0"))(_.last.toInt) ==
      Vector("10-2", "B-2", "1-1", "B-1", "A-0"))

  "empty" in:
    val empty = Prioritized.empty(Map(A(7) -> 1))
    assert(empty.remove(A(3)) == empty)
    assert(empty.selectNext(_ => true) == None)
    assert(empty.selectNext(_ => false) == None)

  "Prioritized" in:
    val aToPriority: A => Int = Map(
      A(32) -> 3,
      A(31) -> 3,
      A(30) -> 3,
      A(12) -> 1,
      A(11) -> 1,
      A(99) -> 9)

    val prioritized = Prioritized.empty(aToPriority)
      .add(A(32))
      .add(A(31))
      .add(A(30))
      .add(A(12))
      .add(A(11))
      .add(A(99))
    assert(prioritized.selectNext(_ => false) == None)


    // Highest priority 9

    assert(prioritized.selectNext(_ => true) == Some(A(99)))
    assert(prioritized.selectNext(_ => true) == Some(A(99)))

    // Next priority, rotating through multiple entries
    assert(prioritized.selectNext(_ != A(99)) == Some(A(32)))
    assert(prioritized.selectNext(_ != A(99)) == Some(A(31)))  // entries are reversed!
    assert(prioritized.selectNext(_ != A(99)) == Some(A(30)))
    assert(prioritized.selectNext(_ != A(99)) == Some(A(32)))   // Priority 3 entries are in original order again!
    assert(prioritized.selectNext(a => Set(11, 12)(a.number)) == Some(A(11)))
    assert(prioritized.selectNext(a => Set(11, 12)(a.number)) == Some(A(12)))
    assert(prioritized.selectNext(a => Set(31, 12)(a.number)) == Some(A(31)))
    assert(prioritized.selectNext(a => Set(31, 12)(a.number)) == Some(A(31)))

  "selectNext" in:
    val prioritized = Prioritized
      .empty(toPriority = (1 to 3).map(i => A(i) -> i).toMap)
      .add(A(1))
      .add(A(2))
      .add(A(3))

    assert(prioritized.selectNext(_ => true) == Some(A(3)))

    val result =
      for
        i <- 1 to 3
        j <- 1 to 3
      yield
        prioritized.remove(A(i)).selectNext(_ != A(j))
    assert(result == Seq(
      Some(A(3)), Some(A(3)), Some(A(2)),
      Some(A(3)), Some(A(3)), Some(A(1)),
      Some(A(2)), Some(A(1)), Some(A(2))))

    pending


private object PrioritizedTest:
  private final case class A(number: Int)
