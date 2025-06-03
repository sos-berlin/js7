package js7.base.utils

import cats.syntax.all.*
import js7.base.test.OurTestSuite

final class CatsTest extends OurTestSuite:

  "sequence" in:
    assert(List(Left(1), Right(2), Left(3)).sequence == Left(1))
    assert(List(Right(1), Right(2), Right(3)).sequence == Right(List(1, 2, 3)))


  "foldMap, foldMapA, foldMapM" - {
    val mixed = List("1", "x", "3", "y")
    val numbers = List("1", "2", "3")

    def toInt(a: String) =
      try Right(a.toInt)
      catch case _: NumberFormatException => Left(s"Not a number: $a")

    "foldMap" in:
      // foldMap is for a Foldable
      assert(mixed.foldMap(toInt) == Left(s"Not a number: x"))
      assert(numbers.foldMap(toInt) == Right(6))

    "foldMapM" in:
      // foldMap is for a Monad
      assert(mixed.foldMapM(toInt) == Left(s"Not a number: x"))
      assert(numbers.foldMapM(toInt) == Right(6))

    "foldMapA" in:
      // foldMap is for an Applicative
      assert(mixed.foldMapA(toInt) == Left(s"Not a number: x"))
      assert(numbers.foldMapA(toInt) == Right(6))
  }
