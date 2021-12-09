package js7.agent.subagent

import js7.agent.subagent.Prioritized.prioritySort
import js7.agent.subagent.PrioritizedTest._
import js7.base.utils.ScalaUtils.syntax.RichEither
import org.scalatest.freespec.AnyFreeSpec

final class PrioritizedTest extends AnyFreeSpec
{
  "prioritySort" in {
    assert(prioritySort(Vector.empty[String])(_ => 0).isEmpty)
    assert(prioritySort(Vector("1-1", "10-2", "B-1", "B-2", "A-0"))(_.last.toInt) ==
      Vector("10-2", "B-2", "1-1", "B-1", "A-0"))
  }

  "Prioritized" in {
    assert(empty.remove(Key(3)) == empty)

    val prioritized = empty
      .insert(Value(Key(32), "DREI", 3)).orThrow
      .insert(Value(Key(31), "TRE", 3)).orThrow
      .insert(Value(Key(30), "THREE", 3)).orThrow
      .insert(Value(Key(12), "EINS", 1)).orThrow
      .insert(Value(Key(11), "ETT", 1)).orThrow
      .insert(Value(Key(99), "HIGH", 9)).orThrow

    val fixedPriority = new FixedPriority

    assert(prioritized.selectNext(fixedPriority, _ => false) == None)


    // Highest priority 9

    assert(prioritized.selectNext(fixedPriority, _ => true) == Some(
      Value(Key(99), "HIGH", 9)))

    assert(prioritized.selectNext(fixedPriority, _ => true) == Some(
      Value(Key(99), "HIGH", 9)))


    // Next priority, rotating through multiple entries

    assert(prioritized.selectNext(fixedPriority, _.key != Key(99)) == Some(
      Value(Key(32), "DREI", 3)))

    assert(prioritized.selectNext(fixedPriority, _.key != Key(99)) == Some(
      Value(Key(31), "TRE", 3)))  // entries are reversed!

    assert(prioritized.selectNext(fixedPriority, _.key != Key(99)) == Some(
      Value(Key(30), "THREE", 3)))

    assert(prioritized.selectNext(fixedPriority, _.key != Key(99)) == Some(
      Value(Key(32), "DREI", 3)))   // Priority 3 entries are in original order again!

    assert(prioritized.selectNext(fixedPriority, v => Set(11, 12)(v.key.number)) == Some(
      Value(Key(11), "ETT", 1)))

    assert(prioritized.selectNext(fixedPriority, v => Set(11, 12)(v.key.number)) == Some(
      Value(Key(12), "EINS", 1)))

    assert(prioritized.selectNext(fixedPriority, v => Set(31, 12)(v.key.number)) == Some(
      Value(Key(31), "TRE", 3)))

    assert(prioritized.selectNext(fixedPriority, v => Set(31, 12)(v.key.number)) == Some(
      Value(Key(31), "TRE", 3)))
  }
}

private object PrioritizedTest
{
  final case class Key(number: Int)
  final case class Value(key: Key, string: String, priority: Int)

  private val empty = Prioritized.empty[Key, Value](_.key, _.priority)
}
