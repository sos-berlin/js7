package com.sos.scheduler.engine.common.scalautil

import ScalaCollections._
import ScalaCollectionsTest._
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
final class ScalaCollectionsTest extends FreeSpec {

  "duplicateKeys" in {
    def dup(o: Seq[A]) = o duplicates { _.i }

    dup(Seq[A]()) shouldBe 'empty
    dup(Seq(a1)) shouldBe 'empty
    dup(Seq(a1, b1)) shouldBe 'empty
    dup(Seq(a1, b1, c1)) shouldBe 'empty
    dup(Seq(a1, a1)) shouldEqual Map(1 -> Seq(a1, a1))
    dup(Seq(a1, a2)) shouldEqual Map(1 -> Seq(a1, a2))
    dup(Seq(a1, a2, b1)) shouldEqual Map(1 -> Seq(a1, a2))
    dup(Seq(a1, a2, b1, c1, c2, c3)) shouldEqual Map(1 -> Seq(a1, a2), 3 -> Seq(c1, c2, c3))
  }

  "requireDistinct" in {
    def r(o: Seq[A]) = o requireDistinct { _.i }

    r(Seq[A]()) shouldBe 'empty
    intercept[Exception] { r(Seq(a1, a2)) }
  }

  "toSeqMultiMap" in {
    List(1 -> 11, 2 -> 22, 3 -> 33, 2 -> 222).toSeqMultiMap shouldEqual Map(1 -> List(11), 2 -> List(22, 222), 3 -> List(33))
  }

  "emptyToNone" in {
    emptyToNone("") shouldEqual None
    emptyToNone("x") shouldEqual Some("x")
    emptyToNone(Nil) shouldEqual None
    emptyToNone(List(1)) shouldEqual Some(List(1))
    emptyToNone(Array[Int]()) shouldEqual None
    val a = Array(1)
    emptyToNone(a) shouldEqual Some(a)
  }
}

private object ScalaCollectionsTest {
  private case class A(i: Int, s: String)

  private val a1 = A(1, "eins")
  private val a2 = A(1, "ett")
  private val b1 = A(2, "zwei")
  private val c1 = A(3, "drei")
  private val c2 = A(3, "tre")
  private val c3 = A(3, "tri")
}
