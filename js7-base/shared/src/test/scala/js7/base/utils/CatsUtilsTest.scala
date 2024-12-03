package js7.base.utils

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, NonEmptySeq, Validated}
import cats.instances.int.*
import cats.instances.string.*
import cats.syntax.traverse.*
import js7.base.catsutils.CatsEffectExtensions.joinStd
import js7.base.problem.{Problem, ProblemException}
import js7.base.test.OurAsyncTestSuite
import js7.base.utils.CatsUtils.*
import js7.base.utils.CatsUtils.syntax.traverseFlat

/**
  * @author Joacim Zschimmer
  */
final class CatsUtilsTest extends OurAsyncTestSuite:

  "combine" in:
    assert(combine(1, 2, 3, 4) == 10)
    assert(combine("a", "-", "b") == "a-b")

  "Validated[Throwable, _] orThrow" in:
    assert((Valid(1): Validated[Throwable, Int]).orThrow == 1)

    val invalid: Validated[Throwable, Int] = Invalid(new IllegalArgumentException("THROWABLE"))
    assert(intercept[IllegalArgumentException](invalid.orThrow).getMessage == "THROWABLE")

  "Validated[Problem, _] orThrow" in:
    assert((Valid(1): Validated[Problem, Int]).orThrow == 1)

    val invalid: Validated[Problem, Int] = Invalid(Problem("PROBLEM"))
    assert(intercept[ProblemException](invalid.orThrow).getMessage == "PROBLEM")

  "NonEmptyList.checked" in:
    assert(NonEmptyList.checked[Int](Seq()) == Left(Problem(
      "Cannot create NonEmptyList[scala.Int] from empty sequence")))
    assert(NonEmptyList.checked[Int](Seq(1)) == Right(NonEmptyList(1, Nil)))
    assert(NonEmptyList.checked[Int](Seq(1, 2)) == Right(NonEmptyList(1, 2 :: Nil)))
    assert(NonEmptyList.checked[Int](Seq(1, 2, 3)) == Right(NonEmptyList(1, 2 :: 3 :: Nil)))

  "NonEmptySeq.checked" in:
    assert(NonEmptySeq.checked[Int](Seq()) == Left(Problem(
      "Cannot create NonEmptySeq[scala.Int] from empty sequence")))
    assert(NonEmptySeq.checked[Int](Seq(1)) == Right(NonEmptySeq(1, Nil)))
    assert(NonEmptySeq.checked[Int](Seq(1, 2)) == Right(NonEmptySeq(1, 2 :: Nil)))
    assert(NonEmptySeq.checked[Int](Seq(1, 2, 3)) == Right(NonEmptySeq(1, 2 :: 3 :: Nil)))

  "pureFiberIO" in:
    val fiber = pureFiberIO(7)
    for result <- fiber.joinStd yield
      assert(result == 7)


  "F[A]" - {
    "traverseFlat(f) == traverse(f).map(_.flatten)" in:
      def f(i: Int): Either[String, List[Int]] =
        if i < 0 then
          Left("NEGATIVE")
        else
          Right(List.fill(i)(i))

      assert(List(1, -2, 3).traverseFlat(f) == Left("NEGATIVE"))
      assert(List(1, -2, 3).traverse(f).map(_.flatten) == Left("NEGATIVE"))

      assert(List(1, 2, 3).traverseFlat(f) == Right(List(1, 2, 2, 3, 3, 3)))
      assert(List(1, 2, 3).traverse(f).map(_.flatten) == Right(List(1, 2, 2, 3, 3, 3)))


    "traverseFlat: inner F must be the same type as outer F" in:
      // Because .flatten requires this :(
      // Maybe traverseFlat is not very usable

      def g(i: Int): Either[String, Vector[Int]] =
        if i < 0 then
          Left("NEGATIVE")
        else
          Right(Vector.fill(i)(i))

      assertDoesNotCompile("List(1, -2, 3).traverseFlat(g)")
      assert(List(1, -2, 3).traverse(g).map(_.flatten) == Left("NEGATIVE"))

      assertDoesNotCompile("List(1, 2, 3).traverseFlat(g)")
      assert(List(1, 2, 3).traverse(g).map(_.flatten) == Right(List(1, 2, 2, 3, 3, 3)))
  }
