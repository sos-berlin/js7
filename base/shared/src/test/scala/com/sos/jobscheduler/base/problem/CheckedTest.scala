package com.sos.jobscheduler.base.problem

import cats.data.Validated.{Invalid, Valid}
import cats.instances.int._
import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.option._
import cats.syntax.traverse._
import com.sos.jobscheduler.base.problem.Checked._
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class CheckedTest extends FreeSpec  {

  "===" in {
    assert(Checked(1) === Checked(1))
    assert(Checked(Problem("X")) === Checked(Problem.fromEagerThrowable(new IllegalArgumentException("X"))))
  }

  "fromOption" in {
    assert(Checked.fromOption(1.some, Problem("PROBLEM")) == Valid(1))
    assert(Checked.fromOption(none, Problem("PROBLEM")) == Invalid(Problem("PROBLEM")))
  }

  "catchNonFatal" in {
    assert(Checked.catchNonFatal(7) == Valid(7))
    val t = new IllegalArgumentException("TEST")
    assert(Checked.catchNonFatal(throw t).swap.getOrElse(null).throwable eq t)
  }

  "flatMap" in {
    assert((Invalid(Problem("A")): Checked[String]).flatMap((_: String) ⇒ throw new NotImplementedError) == Invalid(Problem("A")))
    assert((Valid("A"): Checked[String]).flatMap(_ ⇒ Valid(2)) == Valid(2))
  }

  "withProblemKey" in {
    assert(Valid(1).withProblemKey("X") == Valid(1))
    assert(Invalid(Problem("X")).withProblemKey(333) == Invalid(Problem("Problem with '333': X")))
  }

  "mapProblem" in {
    assert(Valid(1).mapProblem(_ ⇒ throw new NotImplementedError) == Valid(1))
    assert(Invalid(Problem("X")).mapProblem(p ⇒ Problem(s"/$p/")) == Invalid(Problem("/X/")))
  }

  "onProblem" in {
    assert(Valid(1).onProblem(_ ⇒ throw new NotImplementedError) == 1.some)
    var flag = false
    assert(Invalid(Problem("X")).onProblem(_ ⇒ flag = true) == none)
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
}
