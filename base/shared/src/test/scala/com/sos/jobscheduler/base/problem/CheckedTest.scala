package com.sos.jobscheduler.base.problem

import cats.data.Validated.{Invalid, Valid}
import cats.syntax.option._
import com.sos.jobscheduler.base.problem.Checked.ops._
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class CheckedTest extends FreeSpec  {

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
    assert(Invalid(Problem("A")).flatMap((_: String) ⇒ throw new NotImplementedError) == Invalid(Problem("A")))
    assert(Valid("A").flatMap(_ ⇒ Valid(2)) == Valid(2))
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
}
