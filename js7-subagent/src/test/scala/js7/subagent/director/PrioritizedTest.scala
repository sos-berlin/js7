package js7.subagent.director

import js7.base.utils.ScalaUtils.syntax._
import js7.subagent.director.Prioritized.prioritySort
import js7.subagent.director.PrioritizedTest._
import org.scalatest.freespec.AnyFreeSpec

final class PrioritizedTest extends AnyFreeSpec
{
  "prioritySort" in {
    assert(prioritySort(Vector.empty[String])(_ => 0).isEmpty)
    assert(prioritySort(Vector("1-1", "10-2", "B-1", "B-2", "A-0"))(_.last.toInt) ==
      Vector("10-2", "B-2", "1-1", "B-1", "A-0"))
  }

  "Prioritized" in {
    val empty = Prioritized.empty[A](Map(
      A(32)-> 3,
      A(31)-> 3,
      A(30)-> 3,
      A(12)-> 1,
      A(11)-> 1,
      A(99)-> 9))
    assert(empty.remove(A(3)) == empty)

    val prioritized = empty
      .add(A(32)).orThrow
      .add(A(31)).orThrow
      .add(A(30)).orThrow
      .add(A(12)).orThrow
      .add(A(11)).orThrow
      .add(A(99)).orThrow

    assert(prioritized.selectNext(_ => false) == None)


    // Highest priority 9

    assert(prioritized.selectNext(_ => true) == Some(
      A(99)))

    assert(prioritized.selectNext(_ => true) == Some(
      A(99)))


    // Next priority, rotating through multiple entries

    assert(prioritized.selectNext(_ != A(99)) == Some(
      A(32)))

    assert(prioritized.selectNext(_ != A(99)) == Some(
      A(31)))  // entries are reversed!

    assert(prioritized.selectNext(_ != A(99)) == Some(
      A(30)))

    assert(prioritized.selectNext(_ != A(99)) == Some(
      A(32)))   // Priority 3 entries are in original order again!

    assert(prioritized.selectNext(a => Set(11, 12)(a.number)) == Some(
      A(11)))

    assert(prioritized.selectNext(a => Set(11, 12)(a.number)) == Some(
      A(12)))

    assert(prioritized.selectNext(a => Set(31, 12)(a.number)) == Some(
      A(31)))

    assert(prioritized.selectNext(a => Set(31, 12)(a.number)) == Some(
      A(31)))
  }
}

private object PrioritizedTest
{
  private final case class A(number: Int)
}
