package js7.base.utils

import js7.base.problem.Problem
import js7.base.utils.Collections.*
import js7.base.utils.Collections.implicits.*
import js7.base.utils.CollectionsTest.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers.*
import scala.collection.mutable

final class CollectionsTest extends AnyFreeSpec
{
  "IndexSeq get" in {
    assert(Vector.empty.get(0) == None)
    assert(Vector(1, 2, 3).get(0) == Some(1))
    assert(Vector(1, 2, 3).get(-1) == None)
  }

  "IndexSeq dropLastWhile" in {
    assert(Vector.empty.dropLastWhile((_: Int) => true) == Vector.empty)
    assert(Vector.empty.dropLastWhile((_: Int) => false) == Vector.empty)
    assert(Vector(1, 2, 3).dropLastWhile(_ > 2) == Vector(1, 2))
    assert(Vector(1, 2, 3).dropLastWhile(_ >= 2) == Vector(1))
    assert(Vector(1, 2, 3).dropLastWhile(_ < 2) == Vector(1, 2, 3))
  }

  "countEquals" in {
    Iterator(11, 22, 33, 22, 33, 33).countEquals shouldEqual Map(11 -> 1, 22 -> 2, 33 -> 3)
  }

  "when" in {
    assert(Seq(1, 2, 3).when(true) == List(1, 2, 3))
    assert(Seq(1, 2, 3).unless(true) == Nil)
    assert((List(1, 2, 3).unless(true): List[Int]) == Nil)
    assert(Iterator(1, 2, 3).when(true).toList == List(1, 2, 3))
    assert(Iterator(1, 2, 3).unless(true).toList == Nil)
  }

  "fold_" in {
    assert(Seq[String]().fold_ == "")
    assert(Seq("A", "BB", "CCC").fold_ == "ABBCCC")
  }

  "foldFast" in {
    def op(a: String, b: String) = s"$a+$b"
    assert(Nil.foldFast("0")(op) == "0")
    assert(List("1").foldFast("0")(op) == "1")
    assert(List("1", "2").foldFast("0")(op) == "1+2")
    assert(List("1", "2", "3").foldFast("0")(op) == "1+2+3")
    // For comparison with fold:
    assert(Nil.fold("0")(op) == "0")
    assert(List("1").fold("0")(op) == "0+1")
    assert(List("1", "2").fold("0")(op) == "0+1+2")
    assert(List("1", "2", "3").fold("0")(op) == "0+1+2+3")
  }

  "toCheckedKeyMap, toKeyedMap" in {
    case class X(key: String, value: String)
    val goodList = List(X("A", "eins"), X("B", "zwei"))
    goodList.toCheckedKeyedMap(_.key) shouldEqual Right(Map("A" -> X("A", "eins"), "B" -> X("B", "zwei")))
    goodList.toKeyedMap(_.key) shouldEqual Map("A" -> X("A", "eins"), "B" -> X("B", "zwei"))

    val badList = List(X("A", "eins"), X("A", "uno"))
    assert(badList.toCheckedKeyedMap(_.key) == Left(Problem("Unexpected duplicates: 2×A")))
    def duplicatesToProblem(duplicates: Map[String, Iterable[X]]) = Problem("Unexpected: " + duplicates.view.mapValues(_.toSeq).toMap.toString)
    assert(badList.toCheckedKeyedMap(_.key, duplicatesToProblem) == Left(Problem("Unexpected: Map(A -> List(X(A,eins), X(A,uno)))")))
    assert((badList ::: badList).toCheckedKeyedMap(_.key) == Left(Problem("Unexpected duplicates: 4×A")))
    intercept[DuplicateKeyException] { badList.toKeyedMap(_.key) }
  }

  "mergeConsecutiveElements" in {
    assert(List(1, 2, 2, 3, 3, 3, 4).mergeConsecutiveElements { case (a, b) if a == b => a } ==
      List(1, 2, 3, 4))
    assert(List(1, 1, 2, 2, 2).mergeConsecutiveElements { case (a, b) if a == b => a } ==
      List(1, 2))
    assert(List(11, 12, 21, 22, 23).mergeConsecutiveElements { case (a, b) if a / 10 == b / 10 => a } ==
      List(11, 21))
    assert(List(1, 2, 3, 4, 5).mergeConsecutiveElements { case (a, b) => a + b } ==
      List(15))
  }

  "mapValuesStrict" - {
    var called = 0
    def newF() = new (Int => Int) {
      def apply(i: Int) = {
        called += 1
        2 * i
      }
    }

    "mapValuesStrict is strict" in {
      val f = newF()
      val a = Map(1 -> 10).mapValuesStrict(f)
      assert(called == 1)
      a(1)
      assert(called == 1)
    }
  }

  "uniqueToSet" in {
    List(1, 2, 3).uniqueToSet shouldEqual Set(1, 2, 3)
    intercept[DuplicateKeyException] { List(1, 2, 1).uniqueToSet }
  }

  "retainOrderGroupBy" in {
    case class A(name: String, i: Int)
    val list = List(A("eins", 1), A("zwei a", 2), A("drei", 3), A("vier", 4), A("fünf", 5), A("zwei b", 2))
    list.retainOrderGroupBy(_.i) shouldEqual Vector(
      1 -> Vector(A("eins", 1)),
      2 -> Vector(A("zwei a", 2), A("zwei b", 2)),
      3 -> Vector(A("drei", 3)),
      4 -> Vector(A("vier", 4)),
      5 -> Vector(A("fünf", 5)))
    intercept[DuplicateKeyException] { List(1 -> "eins", 1 -> "ett").toKeyedMap(_._1) }
  }

  "takeWhileInclusive" in {
    var next = 0
    val it = Iterator.continually { next += 1; next }
    assert(it.takeWhileInclusive(_ <= 3).toList == 1 :: 2 :: 3 :: 4 :: Nil)
    assert(next == 4)

    assert(Iterator.empty[Int].takeWhileInclusive(_ <= 3).isEmpty)
    assert(Iterator(1, 1, 1).takeWhileInclusive(_ < 1).toList == 1 :: Nil)
  }

  "duplicate" in {
    assert(Seq.empty[A].duplicates.isEmpty)
    assert(Seq("a").duplicates.isEmpty)
    assert(Seq("a", "b").duplicates.isEmpty)
    assert(Seq("a", "b", "c").duplicates.isEmpty)
    assert(Seq("a", "b", "a").duplicates.toSet == Set("a"))
    assert(Seq("a", "b", "c", "c", "b" ).duplicates.toSet == Set("b", "c"))
  }

  "duplicateKeys" in {
    def dup(o: Seq[A]) = o.duplicateKeys(_.i)

    assert(dup(Seq.empty[A]) == None)
    assert(dup(Seq(a1)) == None)
    assert(dup(Seq(a1, b1)) == None)
    assert(dup(Seq(a1, b1, c1)) == None)
    assert(dup(Seq(a1, a1)) == Some(Map(1 -> Seq(a1, a1))))
    assert(dup(Seq(a1, a2)) == Some(Map(1 -> Seq(a1, a2))))
    assert(dup(Seq(a1, a2, b1)) == Some(Map(1 -> Seq(a1, a2))))
    assert(dup(Seq(a1, a2, b1, c1, c2, c3)) == Some(Map(1 -> Seq(a1, a2), 3 -> Seq(c1, c2, c3))))
  }

  "isUnique" in {
    assert(Nil.areUnique)
    assert(Seq(1, 2, 3).areUnique)
    assert(!Seq(1, 1).areUnique)
  }

  "requireUniqueness" in {
    def r(o: Seq[A]) = o.requireUniqueness(_.i)

    assert(r(Seq[A]()).isEmpty)
    intercept[DuplicateKeyException] { r(Seq(a1, a2)) }

    Nil.requireUniqueness
    intercept[DuplicateKeyException] { List(1, 1).requireUniqueness }
  }

  "checkUniqueness" in {
    def r(o: Seq[A]) = o.checkUniqueness(_.i)

    assert(r(Seq.empty[A]) == Right(Nil))
    assert(r(Seq(a1, a2)) == Left(Problem("Unexpected duplicates: 2×1")))
    assert(r(Seq(a1, a2, a1)) == Left(Problem("Unexpected duplicates: 3×1")))

    assert(List.empty[Int].checkUniqueness == Right(Nil))
    assert(List(1, 1).checkUniqueness == Left(Problem("Unexpected duplicates: 2×1")))
    assert(List(1, 2).checkUniqueness == Right(List(1, 2)))
  }

  "toSeqMultiMap" in {
    List(1 -> 11, 2 -> 22, 3 -> 33, 2 -> 222).toSeqMultiMap shouldEqual Map(1 -> List(11), 2 -> List(22, 222), 3 -> List(33))
  }

  "insert" in {
    val m = mutable.Map(1 -> "eins", 2 -> "zwei")
    m.insert(3, "drei")
    m(3) shouldEqual "drei"
    intercept[DuplicateKeyException] { m.insert(3, "drei") }
  }

  "isDisjoint with" in {
    assert(Set(1, 2) isDisjointWith Set(3, 4))
    assert(!Set(1, 2).isDisjointWith(Set(2, 3)))
    assert(!Set(1, 2).isDisjointWith(Set(1, 2)))
  }

  "emptyToNone" in {
    emptyToNone("") shouldEqual None
    emptyToNone(null: String) shouldEqual None
    emptyToNone("x") shouldEqual Some("x")
    emptyToNone(Nil) shouldEqual None
    emptyToNone(null: Iterable[?]) shouldEqual None
    emptyToNone(List(1)) shouldEqual Some(List(1))
    emptyToNone(Array.empty[Int]) shouldEqual None
    emptyToNone(null: Array[Int]) shouldEqual None
    val a = Array(1)
    emptyToNone(a) shouldEqual Some(a)
  }

  "checkedUniqueToMap, uniqueToMap" in {
    val list = List(1 -> "eins", 2 -> "zwei", 3 -> "drei")
    list.checkedUniqueToMap shouldEqual Right(list.toMap)
    list.uniqueToMap shouldEqual list.toMap

    val badList = List(1 -> "eins", 2 -> "zwei", 1 -> "duplicate")
    assert(badList.checkedUniqueToMap(o => Problem(s"Duplicates: $o")) == Left(Problem("Duplicates: Map(1 -> List(eins, duplicate))")))
    intercept[DuplicateKeyException] { badList.uniqueToMap }
  }

  "compareChain" in {
    assert((Iterator.empty[Int] compareElementWise Iterator()) == 0)
    assert((Iterator(1) compareElementWise Iterator()) > 0)
    assert((Iterator(1) compareElementWise Iterator(1)) == 0)
    assert((Iterator() compareElementWise Iterator(1)) < 0)
    assert((Iterator(1, 2, 3) compareElementWise Iterator(1, 2, 3)) == 0)
    assert((Iterator(1, 2, 3) compareElementWise Iterator(1, 2)) > 0)
    assert((Iterator(1, 2, 3) compareElementWise Iterator(1, 2, 3, 4)) < 0)
    assert((Iterator(3, 2, 3) compareElementWise Iterator(1, 2, 3)) > 0)
    assert((Iterator(0, 2, 3) compareElementWise Iterator(1, 2, 3)) < 0)
  }

  "Map" - {
    "toChecked" in {
      val m = Map(1 -> "A") toChecked (key => Problem(s"NO SUCH KEY: $key"))
      assert(m(1) == Right("A"))
      assert(m(2) == Left(Problem("NO SUCH KEY: 2")))
    }

    "withNoSuchKey" in {
      val m = Map(1 -> "A") withNoSuchKey (key => throw new NoSuchElementException(s"NO SUCH KEY: $key"))
      assert(m(1) == "A")
      assert(intercept[NoSuchElementException] { m(2) }.getMessage == "NO SUCH KEY: 2")
    }
  }
}

private object CollectionsTest {
  private case class A(i: Int, s: String)

  private val a1 = A(1, "eins")
  private val a2 = A(1, "ett")
  private val b1 = A(2, "zwei")
  private val c1 = A(3, "drei")
  private val c2 = A(3, "tre")
  private val c3 = A(3, "tri")
}
