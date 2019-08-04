package com.sos.jobscheduler.base.problem

import cats.data.Validated.{Invalid, Valid}
import cats.instances.all._
import cats.syntax.all._
import cats.{Applicative, Apply}
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import io.circe.generic.JsonCodec
import org.scalatest.FreeSpec
import scala.util.{Failure, Success}

/**
  * @author Joacim Zschimmer
  */
final class CheckedTest extends FreeSpec
{
  "JSON" - {
    import Checked.implicits.{checkedJsonDecoder, checkedJsonEncoder}

    @JsonCodec
    case class A(number: Int)

    "Valid" in {
      testJson[Checked[A]](Valid(A(1)),
        json"""{
          "number": 1
        }""")
    }

    "Invalid" in {
      testJson[Checked[A]](Invalid(Problem("PROBLEM")),
        json"""{
          "TYPE": "Problem",
          "message": "PROBLEM"
        }""")
    }
  }

  "===" in {
    assert(Checked(1) === Checked(1))
    assert(Checked(Problem("X")) === Checked(Problem.pure(new IllegalArgumentException("X"))))
  }

  "fromOption" in {
    assert(Checked.fromOption(1.some, Problem("PROBLEM")) == Valid(1))
    assert(Checked.fromOption(none, Problem("PROBLEM")) == Invalid(Problem("PROBLEM")))
  }

  "flattenTryChecked" in {
    assert(Checked.flattenTryChecked(Success(Valid(1))) == Valid(1))
    assert(Checked.flattenTryChecked(Success(Invalid(Problem("PROBLEM")))) == Invalid(Problem("PROBLEM")))
    val throwable = new RuntimeException("EXCEPTION")
    assert(Checked.flattenTryChecked(Failure(throwable)) == Invalid(Problem("EXCEPTION")))
  }

  "fromTry" in {
    assert(Checked.fromTry(Success(1)) == Valid(1))
    val throwable = new RuntimeException("EXCEPTION")
    assert(Checked.fromTry(Failure(throwable)) == Invalid(Problem("EXCEPTION")))
  }

  "cond" in {
    assert(Checked.cond(true, 1, Problem("PROBLEM")) == Valid(1))
    assert(Checked.cond(false, 1, Problem("PROBLEM")) == Invalid(Problem("PROBLEM")))
  }

  "invalidIf" in {
    assert(Checked.invalidIf(true, Problem("PROBLEM")) == Invalid(Problem("PROBLEM")))
    assert(Checked.invalidIf(false, Problem("PROBLEM")) == Checked.unit)
  }

  "catchNonFatal" in {
    assert(Checked.catchNonFatal(7) == Valid(7))
    val t = new IllegalArgumentException("TEST")
    assert(Checked.catchNonFatal(throw t).swap.getOrElse(null).throwable eq t)
  }

  "catchProblem" in {
    assert(Checked.catchProblem(7) == Valid(7))

    val t = new IllegalArgumentException("TEST")
    intercept[IllegalArgumentException] { Checked.catchProblem(throw t) }

    val problem = Problem("TEST")
    assert(Checked.catchProblem(throw problem.throwable).swap.getOrElse(null) eq problem)
  }

  private val valid1: Checked[Int] = Valid(1)
  private val valid2: Checked[Int] = Valid(2)
  private val invalidX: Checked[Int] = Invalid(Problem("X"))
  private val invalidY: Checked[Int] = Invalid(Problem("Y"))

  "flatMap" in {
    assert((Invalid(Problem("A")): Checked[String]).flatMap((_: String) => throw new NotImplementedError) == Invalid(Problem("A")))
    assert((Valid("A"): Checked[String]).flatMap(_ => Valid(2)) == Valid(2))
  }

  "for-comprehension (flatMap)" in {
    val valid = for {
      a <- valid1
      b <- valid2
    } yield a -> b
    assert(valid == Valid(1 -> 2))
  }

  "for-comprehension (flatMap), fail-fast" in {
    val invalid = for {
      a <- valid1
      b <- invalidX
      c <- valid2
      d <- invalidY
    } yield (a, b, c, d)
    assert(invalid == Invalid(Problem("X")))
  }

  "mapN" in {
    assert((valid1, valid2).mapN((a, b) => (a, b)) == Valid(1 -> 2))
  }

  "mapN combines Problems" in {
    assert((valid1, invalidX, valid2, invalidY).mapN((_, _, _, _) => ()) == Invalid(Problem("X\n & Y")))
  }

  "withProblemKey" in {
    assert(Valid(1).withProblemKey("X") == Valid(1))
    assert(Invalid(Problem("X")).withProblemKey(333) == Invalid(Problem("Problem with '333': X")))
  }

  "mapProblem" in {
    assert(Valid(1).mapProblem(_ => throw new NotImplementedError) == Valid(1))
    assert(Invalid(Problem("X")).mapProblem(p => Problem(s"/$p/")) == Invalid(Problem("/X/")))
  }

  "onProblem" in {
    assert(Valid(1).onProblem(_ => throw new NotImplementedError) == 1.some)
    var flag = false
    assert(Invalid(Problem("X")).onProblem(_ => flag = true) == none)
    assert(flag)
  }

  "onProblemHandle" in {
    assert(Valid(1).onProblemHandle(_ => throw new NotImplementedError) == 1)
    var flag = false
    assert(Problem("X").invalid[Int].onProblemHandle { _ => flag = true; 7 } == 7)
    assert(flag)
  }

  "traverse" in {
    def validate(i: Int): Checked[String] = if ((i % 2) == 0) Valid(i.toString) else Invalid(Problem(s"odd $i"))
    assert(List(1, 2, 3).traverse(validate) == Invalid(Problem.multiple("odd 1", "odd 3")))
    assert(List(2, 4, 6).traverse(validate) == Valid(List("2", "4", "6")))
  }

  "sequence" in {
    assert(List(Valid(1), Valid(2), Valid(3)).sequence[Checked, Int] == Valid(List(1, 2, 3)))
    assert(List(Valid(1), Invalid(Problem("X")), Invalid(Problem("Y"))).sequence[Checked, Int] == Invalid(Problem.multiple("X", "Y")))
  }

  "evert" in {
    assert(Valid(7.some).evert == Some(Valid(7)))
    assert(Valid(None).evert == None)
    assert((Problem("X"): Checked[Option[Int]]).evert == Some(Invalid(Problem("X"))))
    assert(Valid(7.some).evert == Some(Valid(7)))
  }

  "toEitherThrowable" in {
    assert(Checked(1).toEitherThrowable == Right(1))
    val Left(throwable: Throwable) = Invalid(Problem("PROBLEM")).toEitherThrowable
    assert(throwable.getMessage == "PROBLEM")
  }

  "toTry" in {
    assert(Checked(1).toTry == Success(1))
    val Failure(throwable: Throwable) = Invalid(Problem("PROBLEM")).toTry
    assert(throwable.getMessage == "PROBLEM")
  }

  "failFastMap" - {
    "Invalid" in {
      var lastI = 0
      def f(i: Int) = {
        lastI = i
        if (i % 2 == 0) Valid(i) else Invalid(Problem("ODD"))
      }
      assert(List(2, 3, 4).failFastMap(f) == Invalid(Problem("ODD")))
      assert(lastI == 3)
    }

    "Valid" in {
      def g(i: Int): Checked[Int] = Valid(i * 11)
      assert(List(1, 2, 3).failFastMap(g) == Valid(List(11, 22, 33)))
    }
  }

  "Some Cats examples" - {
    val valid1: Checked[Int] = Valid(1)
    val valid2: Checked[Int] = Valid(2)
    val problemA = Problem("A")
    val problemB = Problem("B")
    val problemC = Problem("C")
    val invalidA: Checked[Int] = problemA
    val invalidB: Checked[Int] = problemB

    "product" in {
      assert(Applicative[Checked].product(valid1, valid2) == Valid((1, 2)))
      assert(Applicative[Checked].product(valid1, problemB) == Invalid(problemB))
      assert(Applicative[Checked].product(problemA, valid2) == Invalid(problemA))
      assert(Applicative[Checked].product(problemA, problemB) == Invalid(Problem("A\n & B")))
      assert(Apply      [Checked].product(problemA, problemB) == Invalid(Problem("A\n & B")))
    }

    "productR" in {
      assert(Applicative[Checked].productR(valid1)(valid2) == valid2)
      assert(Applicative[Checked].productR(valid1)(problemB) == Invalid(problemB))
      assert(Applicative[Checked].productR(problemA)(valid2) == Invalid(problemA))
      assert(Applicative[Checked].productR(problemA)(problemB) == Invalid(Problem("A\n & B")))
      assert(Apply      [Checked].productR(problemA)(problemB) == Invalid(Problem("A\n & B")))
    }

    "*> (productR)" in {
      assert((valid1 *> valid2) == valid2)
      assert((valid1 *> invalidB) == invalidB)
      assert((invalidA *> valid2) == invalidA)
      assert((invalidA *> invalidB) == (invalidA |+| invalidB))
    }

    "ap" in {
      val ff: Checked[Int => String] = Valid(_.toString)
      assert(Applicative[Checked].ap(ff)(valid1) == Valid("1"))
      assert(Applicative[Checked].ap(ff)(problemB) == Invalid(problemB))
      assert(Applicative[Checked].ap(problemA)(valid1) == Invalid(problemA))
      assert(Applicative[Checked].ap(problemA)(problemB) == Invalid(Problem("A\n & B")))
      assert(Apply      [Checked].ap(problemA)(problemB) == Invalid(Problem("A\n & B")))
    }

    "<*> (ap)" in {
      val validFf: Checked[Int => String] = Valid(_.toString)
      val invalidFf: Checked[Int => String] = Problem("ff")
      assert((validFf <*> valid1) == Valid("1"))
      assert((validFf <*> invalidB) == Invalid(Problem("B")))
      assert((invalidFf <*> valid1) == Invalid(Problem("ff")))
      assert((invalidFf <*> invalidB) == Invalid(Problem("ff\n & B")))
    }

    "ap2" in {
      val ff: Checked[(Int, Int) => String] = Valid(_.toString + _.toString)
      assert(Applicative[Checked].ap2(ff)(valid1, valid2) == Valid("12"))
      assert(Applicative[Checked].ap2(ff)(valid1, problemB) == Invalid(problemB))
      assert(Applicative[Checked].ap2(ff)(problemA, valid2) == Invalid(problemA))
      assert(Applicative[Checked].ap2(ff)(problemA, problemB) == Invalid(Problem("A\n & B")))
      assert(Applicative[Checked].ap2(problemC)(valid1, valid2) == Invalid(problemC))
      assert(Applicative[Checked].ap2(problemC)(valid1, problemB) == Invalid(Problem("B\n & C")))
      assert(Applicative[Checked].ap2(problemC)(problemA, problemB) == Invalid(Problem("A\n & B\n & C")))
      assert(Apply      [Checked].ap2(problemC)(problemA, problemB) == Invalid(Problem("A\n & B\n & C")))
    }

    "map2" in {
      def f(a: Int, b: Int) = a.toString + b.toString
      assert(Applicative[Checked].map2(valid1, valid2)(f) == Valid("12"))
      assert(Applicative[Checked].map2(valid1, problemB)(f) == Invalid(problemB))
      assert(Applicative[Checked].map2(problemA, valid2)(f) == Invalid(problemA))
      assert(Applicative[Checked].map2(problemA, problemB)(f) == Invalid(Problem("A\n & B")))
      assert(Apply      [Checked].map2(problemA, problemB)(f) == Invalid(Problem("A\n & B")))
    }
  }
}
