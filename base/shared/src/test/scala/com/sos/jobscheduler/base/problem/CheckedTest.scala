package com.sos.jobscheduler.base.problem

import cats.data.Validated.Valid
import cats.instances.all._
import cats.syntax.all._
import cats.{Applicative, Apply}
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import io.circe.generic.JsonCodec
import monix.eval.Coeval
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
      testJson[Checked[A]](Right(A(1)),
        json"""{
          "number": 1
        }""")
    }

    "Invalid" in {
      testJson[Checked[A]](Left(Problem("PROBLEM")),
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
    assert(Checked.fromOption(1.some, Problem("PROBLEM")) == Right(1))
    assert(Checked.fromOption(none, Problem("PROBLEM")) == Left(Problem("PROBLEM")))
  }

  "flattenTryChecked" in {
    assert(Checked.flattenTryChecked(Success(Right(1))) == Right(1))
    assert(Checked.flattenTryChecked(Success(Left(Problem("PROBLEM")))) == Left(Problem("PROBLEM")))
    val throwable = new RuntimeException("EXCEPTION")
    assert(Checked.flattenTryChecked(Failure(throwable)) == Left(Problem("EXCEPTION")))
  }

  "fromTry" in {
    assert(Checked.fromTry(Success(1)) == Right(1))
    val throwable = new RuntimeException("EXCEPTION")
    assert(Checked.fromTry(Failure(throwable)) == Left(Problem("EXCEPTION")))
  }

  "cond" in {
    assert(Checked.cond(true, 1, Problem("PROBLEM")) == Right(1))
    assert(Checked.cond(false, 1, Problem("PROBLEM")) == Left(Problem("PROBLEM")))
  }

  "invalidIf" in {
    assert(Checked.invalidIf(true, Problem("PROBLEM")) == Left(Problem("PROBLEM")))
    assert(Checked.invalidIf(false, Problem("PROBLEM")) == Checked.unit)
  }

  "catchNonFatal" in {
    assert(Checked.catchNonFatal(7) == Right(7))
    val t = new IllegalArgumentException("TEST")
    assert(Checked.catchNonFatal(throw t).swap.getOrElse(null).throwable eq t)
  }

  "catchProblem" in {
    assert(Checked.catchProblem(7) == Right(7))

    val t = new IllegalArgumentException("TEST")
    intercept[IllegalArgumentException] { Checked.catchProblem(throw t) }

    val problem = Problem("TEST")
    assert(Checked.catchProblem(throw problem.throwable).swap.getOrElse(null) eq problem)
  }

  private val right1: Checked[Int] = Right(1)
  private val right2: Checked[Int] = Right(2)
  private val leftX: Checked[Int] = Left(Problem("X"))
  private val leftY: Checked[Int] = Left(Problem("Y"))

  "flatMap" in {
    assert((Left(Problem("A")): Checked[String]).flatMap((_: String) => throw new NotImplementedError) == Left(Problem("A")))
    assert((Right("A"): Checked[String]).flatMap(_ => Right(2)) == Right(2))
  }

  "for-comprehension (flatMap)" in {
    val valid = for {
      a <- right1
      b <- right2
    } yield a -> b
    assert(valid == Right(1 -> 2))
  }

  "for-comprehension (flatMap), fail-fast" in {
    val left = for {
      a <- right1
      b <- leftX
      c <- right2
      d <- leftY
    } yield (a, b, c, d)
    assert(left == Left(Problem("X")))
  }

  "mapN" in {
    assert((right1, right2).mapN((a, b) => (a, b)) == Right(1 -> 2))
  }

  //"mapN combines Problems" in {
  //  assert((right1, leftX, right2, leftY).mapN((_, _, _, _) => ()) == Left(Problem("X\n & Y")))
  //}

  "mapN fails-fast" in {
    assert((right1, leftX, right2, leftY).mapN((_, _, _, _) => ()) == Left(Problem("X")))
  }

  "withProblemKey" in {
    assert(Right(1).withProblemKey("X") == Right(1))
    assert(Left(Problem("X")).withProblemKey(333) == Left(Problem("Problem with '333': X")))
  }

  "mapProblem" in {
    assert(Right(1).mapProblem(_ => throw new NotImplementedError) == Right(1))
    assert(Left(Problem("X")).mapProblem(p => Problem(s"/$p/")) == Left(Problem("/X/")))
  }

  "onProblem" in {
    assert(Right(1).onProblem(_ => throw new NotImplementedError) == 1.some)
    var flag = false
    assert(Left(Problem("X")).onProblem(_ => flag = true) == none)
    assert(flag)
  }

  "onProblemHandle" in {
    assert(Right(1).onProblemHandle(_ => throw new NotImplementedError) == 1)
    var flag = false
    assert(Left(Problem("X")).onProblemHandle { _ => flag = true; 7 } == 7)
    assert(flag)
  }

  "traverse with Checked" in {
    def check(i: Int): Checked[String] = if ((i % 2) == 0) Right(i.toString) else Left(Problem(s"odd $i"))
    assert(Left(Problem("PROBLEM")).traverse(check) == Right(Left(Problem("PROBLEM"))))
    assert(Right(1).traverse(check) == Left(Problem("odd 1")))
    assert(Right(2).traverse(check) == Right(Right("2")))
  }

  "traverse with List" in {
    def toList(n: Int) = (1 to n).toList
    assert(Left(Problem("PROBLEM")).traverse(toList) == Left(Problem("PROBLEM")) :: Nil)
    assert(Right(2).traverse(toList) == Right(1) :: Right(2) :: Nil)
  }

  "traverse with Coeval" in {
    def toCoeval(n: Int) = Coeval(n)

    val a: Coeval[Checked[Int]] = Left(Problem("PROBLEM")).traverse(toCoeval)
    assert(a.run().toTry == Success(Left(Problem("PROBLEM"))))

    val b: Coeval[Checked[Int]] = Right(1).traverse(toCoeval)
    assert(b.run().toTry == Success(Right(1)))
  }

  "traverse a List" in {
    def check(i: Int): Checked[String] = if ((i % 2) == 0) Right(i.toString) else Left(Problem(s"odd $i"))
    //assert(List(1, 2, 3).traverse(check) == Left(Problem.multiple("odd 1", "odd 3")))
    assert(List(1, 2, 3).traverse(check) == Left(Problem("odd 1")))
    assert(List(2, 4, 6).traverse(check) == Right(List("2", "4", "6")))
  }

  "sequence" in {
    assert(List(Right(1), Right(2), Right(3)).sequence[Checked, Int] == Right(List(1, 2, 3)))
  //assert(List(Right(1), Left(Problem("X")), Left(Problem("Y"))).sequence[Checked, Int] == Left(Problem.multiple("X", "Y")))
    assert(List(Right(1), Left(Problem("X")), Left(Problem("Y"))).sequence[Checked, Int] == Left(Problem("X")))
  }

  "evert" in {
    assert(Right(7.some).evert == Some(Right(7)))
    assert(Right(None).evert == None)
    assert((Problem("X"): Checked[Option[Int]]).evert == Some(Left(Problem("X"))))
    assert(Right(7.some).evert == Some(Right(7)))
  }

  "orElse" in {
    assert(Right(1).orElse(sys.error("???")) == Right(1))
    assert(Left(Problem("PROBLEM")).orElse(Right(1)) == Right(1))
    assert(Left(Problem("FIRST")).orElse(Left(Problem("SECOND"))) == Left(Problem("SECOND")))
  }

  "orThrow" in {
    assert(Right(1).orThrow == 1)
    assert(intercept[ProblemException](Left(Problem("PROBLEM")).orThrow).getMessage == "PROBLEM")
  }

  "toEitherThrowable" in {
    assert(Checked(1).toEitherThrowable == Right(1))
    val Left(throwable: Throwable) = Left(Problem("PROBLEM")).toEitherThrowable
    assert(throwable.getMessage == "PROBLEM")
  }

  "asTry" in {
    assert(Checked(1).asTry == Success(1))
    val Failure(throwable: Throwable) = Left(Problem("PROBLEM")).asTry
    assert(throwable.getMessage == "PROBLEM")
  }

  "traverseAndCombineProblems" in {
    assert(List(Right(1), Right(2)).traverseAndCombineProblems(i => Right(i * 11)) == Right(Seq(11, 22)))
    assert(List(Left(Problem("ONE")), Right(2), Left(Problem("TWO"))).traverseAndCombineProblems(i => Right(i * 11)) ==
      Left(Problem.multiple("ONE", "TWO")))
  }

  "failFastMap" - {
    "Invalid" in {
      var lastI = 0
      def f(i: Int) = {
        lastI = i
        if (i % 2 == 0) Right(i) else Left(Problem("ODD"))
      }
      assert(List(2, 3, 4).failFastMap(f) == Left(Problem("ODD")))
      assert(lastI == 3)
    }

    "Valid" in {
      def g(i: Int): Checked[Int] = Right(i * 11)
      assert(List(1, 2, 3).failFastMap(g) == Right(List(11, 22, 33)))
    }
  }

  "Some Cats examples" - {
    val valid1: Checked[Int] = Right(1)
    val valid2: Checked[Int] = Right(2)
    val problemA = Problem("A")
    val problemB = Problem("B")
    val problemC = Problem("C")
    val invalidA: Checked[Int] = problemA
    val invalidB: Checked[Int] = problemB

    "product" in {
      assert(Applicative[Checked].product(valid1, valid2) == Right((1, 2)))
      assert(Applicative[Checked].product(valid1, problemB) == Left(problemB))
      assert(Applicative[Checked].product(problemA, valid2) == Left(problemA))
    //assert(Applicative[Checked].product(problemA, problemB) == Left(Problem("A\n & B")))
      assert(Applicative[Checked].product(problemA, problemB) == Left(problemA))
    //assert(Apply      [Checked].product(problemA, problemB) == Left(Problem("A\n & B")))
      assert(Apply      [Checked].product(problemA, problemB) == Left(problemA))
    }

    "productR" in {
      assert(Applicative[Checked].productR(valid1)(valid2) == valid2)
      assert(Applicative[Checked].productR(valid1)(problemB) == Left(problemB))
      assert(Applicative[Checked].productR(problemA)(valid2) == Left(problemA))
    //assert(Applicative[Checked].productR(problemA)(problemB) == Left(Problem("A\n & B")))
      assert(Applicative[Checked].productR(problemA)(problemB) == Left(problemA))
    //assert(Apply      [Checked].productR(problemA)(problemB) == Left(Problem("A\n & B")))
      assert(Apply      [Checked].productR(problemA)(problemB) == Left(problemA))
    }

    "*> (productR)" in {
      assert((valid1 *> valid2) == valid2)
      assert((valid1 *> invalidB) == invalidB)
      assert((invalidA *> valid2) == invalidA)
      assert((invalidA *> invalidB) == (invalidA |+| invalidB))
    }

    "ap" in {
      val ff: Checked[Int => String] = Right(_.toString)
      assert(Applicative[Checked].ap(ff)(valid1) == Right("1"))
      assert(Applicative[Checked].ap(ff)(problemB) == Left(problemB))
      assert(Applicative[Checked].ap(problemA)(valid1) == Left(problemA))
    //assert(Applicative[Checked].ap(problemA)(problemB) == Left(Problem("A\n & B")))
      assert(Applicative[Checked].ap(problemA)(problemB) == Left(problemA))
    //assert(Apply      [Checked].ap(problemA)(problemB) == Left(Problem("A\n & B")))
      assert(Apply      [Checked].ap(problemA)(problemB) == Left(problemA))
    }

    "<*> (ap)" in {
      val validFf: Checked[Int => String] = Right(_.toString)
      val invalidFf: Checked[Int => String] = Problem("ff")
      assert((validFf <*> valid1) == Right("1"))
      assert((validFf <*> invalidB) == Left(Problem("B")))
      assert((invalidFf <*> valid1) == Left(Problem("ff")))
    // assert((invalidFf <*> invalidB) == Left(Problem("ff\n & B")))
      assert((invalidFf <*> invalidB) == Left(Problem("ff")))
    }

    "ap2" in {
      val ff: Checked[(Int, Int) => String] = Right(_.toString + _.toString)
      assert(Applicative[Checked].ap2(ff)(valid1, valid2) == Right("12"))
      assert(Applicative[Checked].ap2(ff)(valid1, problemB) == Left(problemB))
      assert(Applicative[Checked].ap2(ff)(problemA, valid2) == Left(problemA))
    //assert(Applicative[Checked].ap2(ff)(problemA, problemB) == Left(Problem("A\n & B")))
      assert(Applicative[Checked].ap2(ff)(problemA, problemB) == Left(problemA))
      assert(Applicative[Checked].ap2(problemC)(valid1, valid2) == Left(problemC))
    //assert(Applicative[Checked].ap2(problemC)(valid1, problemB) == Left(Problem("B\n & C")))
      assert(Applicative[Checked].ap2(problemC)(valid1, problemB) == Left(problemB))
    //assert(Applicative[Checked].ap2(problemC)(problemA, problemB) == Left(Problem("A\n & B\n & C")))
      assert(Applicative[Checked].ap2(problemC)(problemA, problemB) == Left(problemA))
    //assert(Apply      [Checked].ap2(problemC)(problemA, problemB) == Left(Problem("A\n & B\n & C")))
      assert(Apply      [Checked].ap2(problemC)(problemA, problemB) == Left(problemA))
    }

    "map2" in {
      def f(a: Int, b: Int) = a.toString + b.toString
      assert(Applicative[Checked].map2(valid1, valid2)(f) == Right("12"))
      assert(Applicative[Checked].map2(valid1, problemB)(f) == Left(problemB))
      assert(Applicative[Checked].map2(problemA, valid2)(f) == Left(problemA))
    //assert(Applicative[Checked].map2(problemA, problemB)(f) == Left(Problem("A\n & B")))
      assert(Applicative[Checked].map2(problemA, problemB)(f) == Left(problemA))
    //assert(Apply      [Checked].map2(problemA, problemB)(f) == Left(Problem("A\n & B")))
      assert(Apply      [Checked].map2(problemA, problemB)(f) == Left(problemA))
    }
  }
}
