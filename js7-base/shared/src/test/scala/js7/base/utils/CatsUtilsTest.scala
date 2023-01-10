package js7.base.utils

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, NonEmptySeq, Validated}
import cats.instances.int.*
import cats.instances.string.*
import js7.base.problem.{Problem, ProblemException}
import js7.base.test.OurTestSuite
import js7.base.utils.CatsUtils.*

/**
  * @author Joacim Zschimmer
  */
final class CatsUtilsTest extends OurTestSuite
{
  "combine" in {
    assert(combine(1, 2, 3, 4) == 10)
    assert(combine("a", "-", "b") == "a-b")
  }

  "Validated[Throwable, _] orThrow" in {
    assert((Valid(1): Validated[Throwable, Int]).orThrow == 1)

    val invalid: Validated[Throwable, Int] = Invalid(new IllegalArgumentException("THROWABLE"))
    assert(intercept[IllegalArgumentException](invalid.orThrow).getMessage == "THROWABLE")
  }

  "Validated[Problem, _] orThrow" in {
    assert((Valid(1): Validated[Problem, Int]).orThrow == 1)

    val invalid: Validated[Problem, Int] = Invalid(Problem("PROBLEM"))
    assert(intercept[ProblemException](invalid.orThrow).getMessage == "PROBLEM")
  }

  "NonEmptyList.checked" in {
    assert(NonEmptyList.checked[Int](Seq()) == Left(Problem(
      "Cannot create NonEmptyList[scala.Int] from empty sequence")))
    assert(NonEmptyList.checked[Int](Seq(1)) == Right(NonEmptyList(1, Nil)))
    assert(NonEmptyList.checked[Int](Seq(1, 2)) == Right(NonEmptyList(1, 2 :: Nil)))
    assert(NonEmptyList.checked[Int](Seq(1, 2, 3)) == Right(NonEmptyList(1, 2 :: 3 :: Nil)))
  }

  "NonEmptySeq.checked" in {
    assert(NonEmptySeq.checked[Int](Seq()) == Left(Problem(
      "Cannot create NonEmptySeq[scala.Int] from empty sequence")))
    assert(NonEmptySeq.checked[Int](Seq(1)) == Right(NonEmptySeq(1, Nil)))
    assert(NonEmptySeq.checked[Int](Seq(1, 2)) == Right(NonEmptySeq(1, 2 :: Nil)))
    assert(NonEmptySeq.checked[Int](Seq(1, 2, 3)) == Right(NonEmptySeq(1, 2 :: 3 :: Nil)))
  }

  "continueWithLast" in {
    assert(continueWithLast(1, 2, 3).take(5).toSeq == Seq(1, 2, 3, 3, 3))
  }
}
