package js7.base.utils

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.generic.GenericInt
import js7.base.test.OurTestSuite
import js7.base.utils.Ordinal.*
import js7.base.utils.RangeSet.{Interval, Single}
import js7.base.utils.RangeSetTest.*
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.tester.CirceJsonTester.testJson

final class RangeSetTest extends OurTestSuite {

  "JSON" in {
    import RangeSet.jsonEncoder

    testJson[RangeSet[Int]](
      RangeSet.raw(Vector(Interval(-5, -3), Single(0xffffffff), Single(7), Single(9), Single(10))),
      json""" "-5..-3,-1,7,9,10" """)
  }

  "JSON as Array (not used)" in {
    implicit val jsonEncoder = RangeSet.asArrayJsonEncoder[Int]
    implicit val jsonDecoder = RangeSet.asArrayJsonDecoder[Int]

    testJson[RangeSet[Int]](
      RangeSet.raw(Vector(Single(1), Interval(3, 5), Single(7), Single(9), Single(10))),
      json"""[ 1, [3, 5], 7, 9, 10]""")
  }

  "asString, parseInt" in {
    val rangeSet = RangeSet.fromRanges(Seq(Interval(-5, -3), Interval(-1, 3), Single(7)))
    val string = "-5..-3,-1..3,7"
    assert(rangeSet.asString == string)
    assert(RangeSet.parseInt(string) == Right(rangeSet))
  }

  "fromRanges" in {
    assert(RangeSet.fromRanges[Int](Nil) == RangeSet.raw[Int](Vector.empty))

    assert(RangeSet.fromRanges(Seq(Single(3), Single(1), Single(3), Single(1))) ==
      RangeSet.raw(Vector(Single(1), Single(3))))

    assert(RangeSet.fromRanges(Seq(Interval(1, 3), Interval(1, 2))) ==
      RangeSet.raw(Vector(Interval(1, 3))))

    assert(RangeSet.fromRanges(Seq(Interval(1, 3), Interval(1, 2), Interval(2, 3))) ==
      RangeSet.raw(Vector(Interval(1, 3))))

    // Separate simple Interval 1-2 into 1, 2
    assert(RangeSet.fromRanges(Seq(Interval(1, 2))) ==
      RangeSet.raw(Vector(Single(1), Single(2))))

    assert(RangeSet.fromRanges(Seq(Single(1), Single(2))) ==
      RangeSet.raw(Vector(Single(1), Single(2))))

    assert(RangeSet.fromRanges(Seq(Interval(1, 2), Single(3))) ==
      RangeSet.raw(Vector(Interval(1, 3))))
  }

  "fromRanges coalesces succeeding ranges" in {
    assert(RangeSet.fromRanges(Seq(Interval(1, 2), Interval(3, 4), Single(5), Single(6))) ==
      RangeSet.raw(Vector(Interval(1, 6))))
  }

  "RangeSet[GenericInt]" in {
    assert(RangeSet.fromRanges(Seq(Single(Test(1)), Single(Test(2)))) ==
      RangeSet.raw(Vector(Single(Test(1)), Single(Test(2)))))

    assert(RangeSet.fromRanges(Seq(Single(Test(1)), Single(Test(2)), Interval(Test(3), Test(5)))) ==
      RangeSet.raw(Vector(Interval(Test(1), Test(5)))))
  }

  "RangeSet[Char]" in {
    assert(RangeSet.fromRanges(Seq(Single('a'), Single('b'), Single('c'), Single('x'))) ==
      RangeSet.raw(Vector(Interval('a', 'c'), Single('x'))))
  }

  if (false) "RangeSet[String]" in {
    // Requires an Ordinal[String] with isSuccessorOf
  }

  "Operations" - {
    def toRangeSet(string: String) = RangeSet.parseInt(string).orThrow

    val rangeSet = toRangeSet("1,3..5,7,9..11")
    val set = Set(1, 3, 4, 5, 7, 9, 10, 11)

    "==" in {
      assert(rangeSet.ranges == Vector(Single(1), Interval(3, 5), Single(7), Interval(9, 11)))
      assert(rangeSet == set)
      assert(set == rangeSet)
      assert(toRangeSet("1..999999999") != toRangeSet("1..888888888"))
    }

    "toString" in {
      assert(toRangeSet("-1,1..999999999").toString == "RangeSet(-1,1..999999999)")
    }

    "empty" in {
      assert(RangeSet.empty[Int] == RangeSet.raw[Int](Vector.empty))
    }

    "contains" in {
      assert(!RangeSet.empty[Int].contains(1))

      assert(!rangeSet.contains(0))
      assert(rangeSet.contains(1))
      assert(!rangeSet.contains(2))
      assert(rangeSet.contains(3))
      assert(rangeSet.contains(4))
      assert(rangeSet.contains(5))
      assert(!rangeSet.contains(6))
      assert(rangeSet.contains(7))
      assert(!rangeSet.contains(8))
      assert(rangeSet.contains(9))
      assert(rangeSet.contains(10))
      assert(rangeSet.contains(11))
      assert(!rangeSet.contains(12))
      assert(toRangeSet("1..999999999").contains(400000000))
      assert(toRangeSet("1..999999999")(400000000))
    }

    "+" in {
      assert(rangeSet == rangeSet + 1)
      assert(toRangeSet("0,1,3..5,7,9..11") == rangeSet + 0)
      assert(toRangeSet("1..999999999") + 0 == toRangeSet("0..999999999"))
    }

    "-" in {
      assert(rangeSet - 0 == rangeSet)
      assert(rangeSet - 1 == toRangeSet("3..5,7,9..11"))
      assert(rangeSet - 3 == toRangeSet("1,4,5,7,9..11"))
      assert(rangeSet - 4 == toRangeSet("1,3,5,7,9..11"))
      assert(rangeSet - 5 == toRangeSet("1,3,4,7,9..11"))
      assert(rangeSet - 7 == toRangeSet("1,3..5,9..11"))
      assert(toRangeSet("1..999999999") - 1 == toRangeSet("2..999999999"))
    }

    "subsetOf" in {
      assert(set.subsetOf(rangeSet))
      assert((rangeSet.incl(1)).subsetOf(rangeSet))
      assert(!(rangeSet.incl(0)).subsetOf(rangeSet))
      assert(!(rangeSet.incl(0)).subsetOf(rangeSet))

      assert(toRangeSet("3..5").subsetOf(rangeSet))
      assert(!toRangeSet("2..5").subsetOf(rangeSet))
      assert(!toRangeSet("2..5").subsetOf(rangeSet))
      assert(!toRangeSet("3..6").subsetOf(rangeSet))

      assert(RangeSet.fromRanges(Seq(Interval(3, 5), Single(7))).subsetOf(rangeSet))
      assert(!RangeSet.fromRanges(Seq(Interval(3, 5), Single(8))).subsetOf(rangeSet))

      assert((rangeSet.incl(1)).subsetOf(set))
      assert(!(rangeSet.incl(0)).subsetOf(set))
      assert(!(rangeSet.incl(0)).subsetOf(set))

      assert(toRangeSet("1..888888888").subsetOf(toRangeSet("1..999999999")))
    }

    "🔥 intersect" in {
      assert((rangeSet.intersect(RangeSet(1, 4, 5, 99))/*: RangeSet[Int]*/) ==
        RangeSet.raw(Single(1), Single(4), Single(5)))

      if (false) {
        // Will deplete memory
        assert(toRangeSet("1..999999999").intersect(toRangeSet("1..999999999")) ==
          toRangeSet("1..999999999"))
      }
    }

    "🔥 --" in {
      assert(toRangeSet("1..3") -- toRangeSet("2,5")/*: RangeSet[Int]*/ == toRangeSet("1,3"))
      assert(toRangeSet("2,5") -- toRangeSet("1..3")/*: RangeSet[Int]*/ == toRangeSet("5"))

      if (false) {
        // Will deplete memory
        assert(toRangeSet("1..999999999") -- toRangeSet("500000000..999999998") ==
        toRangeSet("1..499999999 999999999"))
      }
    }

    "🔥 diff" in {
      // Same as -- operator ?
      assert(toRangeSet("1..3").diff(toRangeSet("2,5"))/*: RangeSet[Int]*/ == toRangeSet("1,3"))
      assert(toRangeSet("2,5").diff(toRangeSet("1..3"))/*: RangeSet[Int]*/ == toRangeSet("5"))
      if (false) {
        // Will deplete memory
        assert(toRangeSet("1..999999999").diff(toRangeSet("500000000..999999998")) ==
          toRangeSet("1..499999999,999999999"))
      }
    }

    "union, concat, ++" in {
      assert((RangeSet(2, 6).union(rangeSet)/*: RangeSet[Int]*/) == toRangeSet("1..7,9..11"))
      assert((RangeSet(2, 6).concat(rangeSet): RangeSet[Int]) == toRangeSet("1..7,9..11"))
      assert((rangeSet ++ RangeSet(2, 6)/*: RangeSet[Int]*/) == toRangeSet("1..7,9..11"))
      assert((RangeSet(2, 6) ++ rangeSet/*: RangeSet[Int]*/) == toRangeSet("1..7,9..11"))

      assert(toRangeSet("1..499999999").union(toRangeSet("500000000..999999999")) ==
        toRangeSet("1..999999999"))
      assert(toRangeSet("1..499999999").concat(toRangeSet("500000000..999999999")) ==
        toRangeSet("1..999999999"))
      assert(toRangeSet("1..499999999") ++ toRangeSet("500000000..999999999") ==
        toRangeSet("1..999999999"))
    }
  }
}

object RangeSetTest {
  private final case class Test(number: Int) extends GenericInt
  private object Test extends GenericInt.Companion[Test]
}
