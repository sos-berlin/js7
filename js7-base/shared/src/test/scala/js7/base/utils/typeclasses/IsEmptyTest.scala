package js7.base.utils.typeclasses

import cats.instances.option._
import cats.{Eq, Monoid}
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.typeclasses.IsEmpty.syntax._
import org.scalatest.freespec.AnyFreeSpec

final class IsEmptyTest extends AnyFreeSpec
{
  private val emptyMap = Map.empty[Int, String]
  private val nonEmptyMap = Map(1 -> "")

  private val emptySeq = Seq.empty[Int]
  private val nonEmptySeq = Seq(1)

  private val emptySpecial = Special(0)
  private val nonEmptySpecial = Special(1)

  private val none: Option[Int]= None
  private val some: Option[Int]= Some(1)

  private case class Special(number: Int)
  private implicit val specialMonoid: Monoid[Special] = new Monoid[Special] {
    val empty = Special(0)
    def combine(x: Special, y: Special) = Special(x.number + y.number)
  }
  private implicit val specialEq = Eq.fromUniversalEquals[Special]

  private def testIsEmpty[A](what: A)(implicit A: IsEmpty[A]): Boolean =
    A.isEmpty(what)

  "isEmpty" in {
    assert(testIsEmpty(emptyMap))
    assert(!testIsEmpty(nonEmptyMap))
    assert(testIsEmpty(Map.empty/*no types*/))

    assert(testIsEmpty(""))
    assert(!testIsEmpty("X"))

    assert(testIsEmpty(emptySeq))
    assert(!testIsEmpty(nonEmptySeq))

    assert(testIsEmpty(none))
    assert(!testIsEmpty(some))

    assert(testIsEmpty(emptySpecial))
    assert(!testIsEmpty(nonEmptySpecial))
  }

  "??" in {
    assert(emptyMap.?? == None)
    assert(Map(1 -> "").?? == Some(Map(1 -> "")))

    assert("".?? == None)
    assert("X".?? == Some("X"))

    assert(emptySeq.?? == None)
    assert(nonEmptySeq.?? == Some(nonEmptySeq))

    assert(none.?? == None)
    assert(some.?? == Some(some))  // !!!

    assert(emptySpecial.?? == None)
    assert(nonEmptySpecial.?? == Some(nonEmptySpecial))
  }

  "IsEmpty ?? is compatible with Boolean ??" in {
    assert(true ?? "x" == "x")
    assert(false ?? "x" == "")
  }
}
