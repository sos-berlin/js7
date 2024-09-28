package js7.base.utils.typeclasses

import cats.instances.option.*
import cats.{Eq, Monoid}
import js7.base.test.OurTestSuite
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.typeclasses.IsEmpty.syntax.*

final class IsEmptyTest extends OurTestSuite:
  private val emptyMap = Map.empty[Int, String]
  private val nonEmptyMap = Map(1 -> "")

  private val emptySeq = Seq.empty[Int]
  private val nonEmptySeq = Seq(1)

  private val emptySpecial = Special(0)
  private val nonEmptySpecial = Special(1)

  private val none: Option[Int]= None
  private val some: Option[Int]= Some(1)

  private case class Special(number: Int)
  private object Special:
    implicit val specialMonoid: Monoid[Special] = new Monoid[Special]:
      val empty = Special(0)
      def combine(x: Special, y: Special) = Special(x.number + y.number)
    implicit val specialEq: Eq[Special] = Eq.fromUniversalEquals

  private def testIsEmpty[A](what: A)(implicit A: IsEmpty[A]): Boolean =
    A.isEmpty(what)

  "isEmpty" in:
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

  "monadic ??, ifNonEmpty" in:
    assert(emptyMap.?? == None)
    assert(nonEmptyMap.?? == Some(nonEmptyMap))

    assert("".?? == None)
    assert("X".?? == Some("X"))

    assert(emptySeq.?? == None)
    assert(nonEmptySeq.?? == Some(nonEmptySeq))

    assert(none.?? == None)
    assert(some.?? == Some(some))  // !!!

    // Boolean ?? operator conflicts with dyadic operator (see test below)
    assert(false.ifNonEmpty == None)
    assert(true.ifNonEmpty == Some(true))

    assert(0.?? == None)
    assert(1.?? == Some(1))

    assert("".?? == None)
    assert("x".?? == Some("x"))

    assert(emptySpecial.?? == None)
    assert(nonEmptySpecial.?? == Some(nonEmptySpecial))

  "dyadic ??, ifEmpty" in:
    assert(emptyMap ?? nonEmptyMap == nonEmptyMap)
    assert(emptyMap.ifEmpty(nonEmptyMap) == nonEmptyMap)
    assert(nonEmptyMap.??(Map(7 -> "X")) == nonEmptyMap)
    assert(nonEmptyMap.ifEmpty(Map(7 -> "X")) == nonEmptyMap)

    assert("" ?? "A" == "A")
    assert("".ifEmpty("A") == "A")
    assert("X" ?? "A" == "X")
    assert("X".ifEmpty("A") == "X")

    assert(emptySeq ?? Seq(7) == Seq(7))
    assert(emptySeq.ifEmpty(Seq(7)) == Seq(7))
    assert(nonEmptySeq ?? Seq(7) == nonEmptySeq)
    assert(nonEmptySeq.ifEmpty(Seq(7)) == nonEmptySeq)

    assert(none ?? Some(7) == Some(7))
    assert(none.ifEmpty(Some(7)) == Some(7))
    assert(some ?? Some(7) == some)
    assert(some.ifEmpty(Some(7)) == some)

    // Boolean ?? operator conflicts with dyadic operator (see test below)
    //assert(false ?? true == true)
    assert(false.ifEmpty(true) == true)
    //assert(true ?? false == true)
    assert(true.ifEmpty(false) == true)

    assert(0 ?? 7 == 7)
    assert(0.ifEmpty(7) == 7)
    assert(1 ?? 7 == 1)
    assert(1.ifEmpty(7) == 1)

    assert("" ?? "-" == "-")
    assert("".ifEmpty("-") == "-")
    assert("x" ?? "-" == "x")
    assert("x".ifEmpty("-") == "x")

    assert(emptySpecial ?? Special(7) == Special(7))
    assert(emptySpecial.ifEmpty(Special(7)) == Special(7))
    assert(nonEmptySpecial ?? Special(7) == nonEmptySpecial)
    assert(nonEmptySpecial.ifEmpty(Special(7)) == nonEmptySpecial)

  "Monadic boolean ?? operator and dyadic ?? operator" in:
    // Boolean ?? conflicts with dyadic operator
    //assert(false.?? == None)
    //assert(true.?? == Some(true))

    // But there is the ? operator
    assert(false.? == None)
    assert(true.? == Some(true))

    // The dyadic operator
    assert(true ?? "x" == "x")
    assert(false ?? "x" == "")
