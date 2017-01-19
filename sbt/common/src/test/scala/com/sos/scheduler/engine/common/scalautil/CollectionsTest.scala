package com.sos.scheduler.engine.common.scalautil

import com.sos.scheduler.engine.common.scalautil.Collections._
import com.sos.scheduler.engine.common.scalautil.Collections.implicits._
import com.sos.scheduler.engine.common.scalautil.CollectionsTest._
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import scala.collection.{immutable, mutable}

@RunWith(classOf[JUnitRunner])
final class CollectionsTest extends FreeSpec {

  "toImmutableSeq of an already immutable.Seq" in {
    val list = List(1, 2, 3)
    assert(list.toImmutableSeq eq list)
  }

  "toImmutableIterable" in {
    val seq = mutable.Seq(1, 2, 3)
    assert((seq.toImmutableIterable: immutable.Iterable[Int]) == seq.toVector)
  }

  "toImmutableIterable of an already immutable.Iterable" in {
    val list = List(1, 2, 3)
    assert(list.toImmutableIterable eq list)
  }

  "toImmutableSeq" in {
    val array = Array(1, 2, 3)
    assert(array.toImmutableSeq.isInstanceOf[Vector[_]])
    assert(array.toImmutableSeq == array.toVector)

    val javaList = new java.util.ArrayList[Int]()
    javaList.add(1)
    javaList.add(2)
    javaList.add(3)
    assert(javaList.toImmutableSeq.isInstanceOf[Vector[_]])
    assert(javaList.toImmutableSeq == Vector(1, 2, 3))

    val iterator: java.util.Iterator[Int] = javaList.iterator()
    val iseq = iterator.toImmutableSeq
    assert(iseq.isInstanceOf[Vector[_]])
    assert(iseq == Vector(1, 2, 3))
  }

  "countEquals" in {
    Iterator(11, 22, 33, 22, 33, 33).countEquals shouldEqual Map(11 → 1, 22 → 2, 33 → 3)
    Map[Int, Int]().countEquals shouldEqual Map()
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

  "toKeyedMap" in {
    case class A(name: String, i: Int)
    List(A("eins", 1), A("zwei", 2)) toKeyedMap { _.i } shouldEqual Map(1 → A("eins", 1), 2 → A("zwei", 2))
    intercept[DuplicateKeyException] { List(1 → "eins", 1 → "ett") toKeyedMap { _._1 } }
  }

  "retainOrderGroupBy" in {
    case class A(name: String, i: Int)
    val list = List(A("eins", 1), A("zwei a", 2), A("drei", 3), A("vier", 4), A("fünf", 5), A("zwei b", 2))
    (list retainOrderGroupBy { _.i }) shouldEqual Vector(
      1 → Vector(A("eins", 1)),
      2 → Vector(A("zwei a", 2), A("zwei b", 2)),
      3 → Vector(A("drei", 3)),
      4 → Vector(A("vier", 4)),
      5 → Vector(A("fünf", 5)))
    intercept[DuplicateKeyException] { List(1 → "eins", 1 → "ett") toKeyedMap { _._1 } }
  }

  "duplicateKeys" in {
    def dup(o: Seq[A]) = o duplicates { _.i }

    dup(Seq[A]()) shouldBe 'empty
    dup(Seq(a1)) shouldBe 'empty
    dup(Seq(a1, b1)) shouldBe 'empty
    dup(Seq(a1, b1, c1)) shouldBe 'empty
    dup(Seq(a1, a1)) shouldEqual Map(1 → Seq(a1, a1))
    dup(Seq(a1, a2)) shouldEqual Map(1 → Seq(a1, a2))
    dup(Seq(a1, a2, b1)) shouldEqual Map(1 → Seq(a1, a2))
    dup(Seq(a1, a2, b1, c1, c2, c3)) shouldEqual Map(1 → Seq(a1, a2), 3 → Seq(c1, c2, c3))
  }

  "requireUniqueness" in {
    def r(o: Seq[A]) = o requireUniqueness { _.i }

    r(Seq[A]()) shouldBe 'empty
    intercept[DuplicateKeyException] { r(Seq(a1, a2)) }

    Nil.requireUniqueness
    intercept[DuplicateKeyException] { List(1, 1).requireUniqueness }
  }

  "toSeqMultiMap" in {
    List(1 → 11, 2 → 22, 3 → 33, 2 → 222).toSeqMultiMap shouldEqual Map(1 → List(11), 2 → List(22, 222), 3 → List(33))
  }

  "insert" in {
    val m = mutable.Map(1 → "eins", 2 → "zwei")
    m.insert(3 → "drei")
    m(3) shouldEqual "drei"
    intercept[DuplicateKeyException] { m.insert(3 → "drei") }
  }

  "emptyToNone" in {
    emptyToNone("") shouldEqual None
    emptyToNone(null: String) shouldEqual None
    emptyToNone("x") shouldEqual Some("x")
    emptyToNone(Nil) shouldEqual None
    emptyToNone(null: Iterable[_]) shouldEqual None
    emptyToNone(List(1)) shouldEqual Some(List(1))
    emptyToNone(Array[Int]()) shouldEqual None
    emptyToNone(null: Array[Int]) shouldEqual None
    val a = Array(1)
    emptyToNone(a) shouldEqual Some(a)
  }

  "java.util.stream.Stream.toImmutableSeq" in {
    java.util.stream.Stream.of(1, 2, 3).toImmutableSeq shouldEqual List(1, 2, 3)
  }

  "java.util.stream.Stream.toVector" in {
    java.util.stream.Stream.of(1, 2, 3).toVector shouldEqual Vector(1, 2, 3)
  }

  "java.util.stream.Stream.toSet" in {
    java.util.stream.Stream.of(1, 2, 2, 3).toSet shouldEqual Set(1, 2, 3)
  }

  "java.util.stream.Stream.toIterator" in {
    for ((a, b) ← java.util.stream.Stream.of(1, 2, 3).toIterator zip Iterator(1, 2, 3))
      assert(a == b)
  }

  "java.util.stream.Stream.toIterable" in {
    assert(java.util.stream.Stream.of(1, 2, 3).toIterable == Iterable(1, 2, 3))
  }

  "java.util.stream.Stream.toStream" in {
    assert(java.util.stream.Stream.of(1, 2, 3).toStream == Stream(1, 2, 3))
  }

  "uniqueToMap" in {
    val list = List(1 → "eins", 2 → "zwei", 3 → "drei")
    list.uniqueToMap shouldEqual list.toMap
    intercept[DuplicateKeyException] { List(1 → "eins", 2 → "zwei", 1 → "duplicate").uniqueToMap }
  }

  "compareChain" in {
    assert((Iterator[Int]() compareElementWise Iterator()) == 0)
    assert((Iterator(1) compareElementWise Iterator()) > 0)
    assert((Iterator(1) compareElementWise Iterator(1)) == 0)
    assert((Iterator() compareElementWise Iterator(1)) < 0)
    assert((Iterator(1, 2, 3) compareElementWise Iterator(1, 2, 3)) == 0)
    assert((Iterator(1, 2, 3) compareElementWise Iterator(1, 2)) > 0)
    assert((Iterator(1, 2, 3) compareElementWise Iterator(1, 2, 3, 4)) < 0)
    assert((Iterator(3, 2, 3) compareElementWise Iterator(1, 2, 3)) > 0)
    assert((Iterator(0, 2, 3) compareElementWise Iterator(1, 2, 3)) < 0)
  }

  "Vector.build" in {
    assert(Vector.build[Int] { builder ⇒ builder += 7} == Vector(7))
  }

  "Map.build" in {
    assert(Map.build[Int, String] { builder ⇒ builder += 7 → "seven"} == Map(7 → "seven"))
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
