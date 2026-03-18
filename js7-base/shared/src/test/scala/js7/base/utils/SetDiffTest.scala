package js7.base.utils

import js7.base.circeutils.CirceUtils.*
import js7.base.test.OurTestSuite
import js7.tester.CirceJsonTester.testJson

final class SetDiffTest extends OurTestSuite:

  "applyTo" in:
    assert(SetDiff.empty.applyTo(Set.empty) == Set.empty)
    val a = SetDiff(added = Set("a", "b"), deleted = Set("c")).applyTo(Set.empty)
    assert(a == Set("a", "b"))

  "diff" in:
    assert(SetDiff.diff(Set.empty, Set.empty) eq SetDiff.empty)
    assert(SetDiff.diff(
      Set("a", "b", "x", "y"),
      Set("a", "b", "c", "d")
    ) == SetDiff(added = Set("c", "d"), deleted = Set("x", "y")))

  "empty"in:
    val m = Set("a")
    assert(SetDiff.empty.applyTo(m) eq m)

  "empty is singleton" in:
    assert(SetDiff(Set.empty, Set.empty) eq SetDiff.empty)

  "empty SetDiff.apply retains the same object" in:
    val m = Set("a")
    assert(SetDiff.empty.applyTo(m) eq m)

  "JSON" in:
    testJson(SetDiff.empty[String],
      json"""{
        "added": [],
        "deleted": []
      }""")

    testJson(SetDiff(added = Set("a", "b"), deleted = Set("d", "D")),
      json"""{
        "added": ["a", "b"],
        "deleted": ["d", "D"]
      }""")
