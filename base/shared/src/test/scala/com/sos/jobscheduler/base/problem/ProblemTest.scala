package com.sos.jobscheduler.base.problem

import cats.syntax.semigroup._
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ProblemTest extends FreeSpec {

  "String" in {
    val problem = Problem("MESSAGE")
    assert(problem.toString == "MESSAGE")
    assert(problem.throwableOption.isEmpty)
    assert(problem.throwable.getMessage == "MESSAGE")

    assert(problem.withKey("KEY").toString == "Problem with 'KEY': MESSAGE")
    assert(problem.withKey("KEY").throwableOption.isEmpty)
    assert(problem.withKey("KEY").throwable.getMessage == "Problem with 'KEY': MESSAGE")
  }

  "fromLazyThrowable" in {
    val throwable = new RuntimeException("MESSAGE")
    val problem = Problem.fromLazyThrowable(throwable)
    assert(problem.toString == "MESSAGE")
    assert(problem.throwableOption contains throwable)
    assert(problem.throwable eq throwable)

    assert(problem.withKey("KEY").toString == "Problem with 'KEY', caused by: MESSAGE")
    assert(problem.withKey("KEY").throwable.getMessage == "Problem with 'KEY':")

    assert(Problem.fromLazyThrowable(new RuntimeException).toString == "java.lang.RuntimeException")
  }

  "combine" in {
    assert((Problem("A") |+| Problem("B")) == Problem("A - B"))
    assert((Problem("A:") |+| Problem("B")) == Problem("A: B"))
    assert((Problem("A:   ") |+| Problem("B")) == Problem("A: B"))
    assert((Problem("A") |+| Problem.fromLazyThrowable(new RuntimeException("B"))).throwableOption.get.getMessage == "A")
    assert((Problem("A") |+| Problem.fromLazyThrowable(new RuntimeException("B"))).throwableOption.get.toStringWithCauses == "A, caused by: B")
    assert((Problem.fromLazyThrowable(new RuntimeException("A")) |+| Problem("B")).throwableOption.get.getMessage == "A")
    assert((Problem.fromLazyThrowable(new RuntimeException("A")) |+| Problem("B")).throwableOption.get.toStringWithCauses == "A, caused by: B")
    assert((Problem.fromLazyThrowable(new RuntimeException("A")) |+| Problem.fromLazyThrowable(new RuntimeException("B"))).throwableOption.get.getMessage == "A")
    assert((Problem.fromLazyThrowable(new RuntimeException("A")) |+| Problem.fromLazyThrowable(new RuntimeException("B"))).throwableOption.get.toStringWithCauses == "A, caused by: B")
  }

  "Problem is lazy" in {
    Problem(throw new Exception)
    Problem(throw new Exception).withKey("KEY")
    intercept[Exception] {
      Problem(throw new Exception).toString
    }
  }

  "equals" in {
    assert(Problem("TEST") == Problem("TEST"))
    assert(Problem("X") != Problem("Y"))
  }
}
