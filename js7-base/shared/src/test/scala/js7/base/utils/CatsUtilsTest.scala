package js7.base.utils

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, NonEmptySeq, Validated}
import cats.effect.IO
import cats.instances.int.*
import cats.instances.string.*
import cats.syntax.traverse.*
import js7.base.catsutils.CatsEffectExtensions.joinStd
import js7.base.problem.{Problem, ProblemException}
import js7.base.test.OurAsyncTestSuite
import js7.base.utils.CatsUtils.*

/**
  * @author Joacim Zschimmer
  */
final class CatsUtilsTest extends OurAsyncTestSuite:

  "whenM" in :
    for
      a <- whenM(false)(IO("*"))
      _ = assert(a == "")
      a <- whenM(true)(IO("*"))
      _ = assert(a == "*")
    yield succeed

  "unlessM" in :
    for
      a <- unlessM(false)(IO("*"))
      _ = assert(a == "*")
      a <- unlessM(true)(IO("*"))
      _ = assert(a == "")
    yield succeed

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
      import js7.base.utils.CatsUtils.syntax.traverseFlat

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

  "View[Either[L, R]]" - {
    def f(i: Int): Either[String, Int] =
      if i % 2 == 1 then Left(s"$i is not even") else Right(100 * i)

    "sequence" in:
      import js7.base.utils.CatsUtils.syntax.sequence

      val lazyList = LazyList.from(1).map(f)
      assert(lazyList.sequence == Left("1 is not even"))

      val list = List(Left(1), Right(2), Left(3), Right(4))
      assert(list.view.sequence == Left(1))
      assert(list.sequence == Left(1))

    "traverse" in:
      import js7.base.utils.CatsUtils.syntax.traverse

      val lazyList = LazyList.from(1)
      assert(lazyList.traverse(f) == Left("1 is not even"))

      val list = List(1, 2, 3, 4)
      assert(list.view.traverse(f) == Left("1 is not even"))
      assert(list.traverse(f) == Left("1 is not even"))
  }
