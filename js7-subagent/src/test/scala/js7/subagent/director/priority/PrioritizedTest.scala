package js7.subagent.director.priority

import js7.base.test.OurTestSuite
import js7.data.value.NumberValue
import js7.subagent.director.priority.Prioritized.groupByPriority
import js7.subagent.director.priority.PrioritizedTest.*

final class PrioritizedTest extends OurTestSuite:

  "groupByPriority" in:
    assert(groupByPriority[String](Vector.empty).isEmpty)
    assert(groupByPriority(Vector(
      "1-1" -> NumberValue(1),
      "10-2" -> NumberValue(2),
      "B-1" -> NumberValue(1),
      "B-2" -> NumberValue(2),
      "A-0" -> NumberValue(0))) ==
      Vector(
        Vector("10-2", "B-2"),
        Vector("1-1", "B-1"),
        Vector("A-0")))

  "empty" in:
    val empty = Prioritized.empty
    assert(empty.selectNext(_ => true) == None)
    assert(empty.selectNext(_ => false) == None)

  "selectNext" in:
    val prioritized = Prioritized(Vector(
      A(32) -> NumberValue(3),
      A(31) -> NumberValue(3),
      A(30) -> NumberValue(3),
      A(12) -> NumberValue(1),
      A(11) -> NumberValue(1),
      A(99) -> NumberValue(9)))
    assert(prioritized.selectNext(_ => false) == None)

    // Highest priority is 9

    assert(prioritized.selectNext(_ => true) == Some(A(99)))
    assert(prioritized.selectNext(_ => false) == None)

    // Ignore A(99): next priority is selected, rotating through multiple entries
    assert(prioritized.selectNext(_ != A(99)) == Some(A(32)))
    assert(prioritized.selectNext(_ != A(99)) == Some(A(31)))  // entries are reversed!
    assert(prioritized.selectNext(_ != A(99)) == Some(A(30)))
    assert(prioritized.selectNext(_ != A(99)) == Some(A(32)))  // Priority 3 entries are in original order again!
    assert(prioritized.selectNext(a => Set(11, 12)(a.int)) == Some(A(12)))
    assert(prioritized.selectNext(a => Set(11, 12)(a.int)) == Some(A(11)))
    assert(prioritized.selectNext(a => Set(31, 12)(a.int)) == Some(A(31)))
    assert(prioritized.selectNext(a => Set(31, 12)(a.int)) == Some(A(31)))

  "isEquvivalent" in :
    assert(Prioritized.empty.isEquivalentTo(Prioritized.empty))

    val prioritized = Prioritized(Vector(
      A(10) -> NumberValue(1),
      A(11) -> NumberValue(1),
      A(20) -> NumberValue(2),
      A(31) -> NumberValue(3)))
    assert(prioritized.isEquivalentTo(prioritized))

    assert(prioritized.isEquivalentTo:
      Prioritized(Vector(
        A(10) -> NumberValue(111),
        A(11) -> NumberValue(111),
        A(20) -> NumberValue(222),
        A(31) -> NumberValue(333))))

    assert(!prioritized.isEquivalentTo:
      Prioritized(Vector(
        A(10) -> NumberValue(111),
        A(11) -> NumberValue(118),
        A(20) -> NumberValue(222),
        A(31) -> NumberValue(333))))


private object PrioritizedTest:
  private final case class A(int: Int)
