package com.sos.jobscheduler.base.problem

import cats.syntax.semigroup._
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.tester.CirceJsonTester
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ProblemTest extends FreeSpec {

  "JSON" in {
    CirceJsonTester.testJson(
      Problem("A problem"),
      json"""{
        "TYPE": "Problem",
        "message": "A problem"
      }""")
  }

  "String" in {
    assert(Problem("").toString == "A problem occurred (no message)")
    assert(Problem(null).toString == "A problem occurred (null)")

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
    assert((Problem("A") |+| Problem("B")) == Problem("A\n & B"))
    assert((Problem("A:") |+| Problem("B")) == Problem("A: B"))
    assert((Problem("A: ") |+| Problem("B")) == Problem("A: B"))
    assert((Problem("A -") |+| Problem("B")) == Problem("A - B"))
    assert((Problem("A - ") |+| Problem("B")) == Problem("A - B"))
    assert((Problem("A") |+| Problem.fromLazyThrowable(new RuntimeException("B"))).throwableOption.get.getMessage == "A")
    assert((Problem("A") |+| Problem.fromLazyThrowable(new RuntimeException("B"))).throwableOption.get.toStringWithCauses == "A, caused by: B")
    assert((Problem.fromLazyThrowable(new RuntimeException("A")) |+| Problem("B")).throwableOption.get.getMessage == "A")
    assert((Problem.fromLazyThrowable(new RuntimeException("A")) |+| Problem("B")).throwableOption.get.toStringWithCauses == "A, caused by: B")
    assert((Problem.fromLazyThrowable(new RuntimeException("A")) |+| Problem.fromLazyThrowable(new RuntimeException("B"))).throwableOption.get.getMessage == "A")
    assert((Problem.fromLazyThrowable(new RuntimeException("A")) |+| Problem.fromLazyThrowable(new RuntimeException("B"))).throwableOption.get.toStringWithCauses == "A, caused by: B")
  }

  "Multiple" in {
    Problem("A") |+| Problem("B") match {
      case Problem.Multiple(problems) ⇒ assert(problems == List(Problem("A"), Problem("B")))
      case _ ⇒ fail()
    }
  }

  "Multiple is flat" in {
    Problem("A") |+| Problem("B") |+| Problem("C") match {
      case Problem.Multiple(problems) ⇒ assert(problems == List(Problem("A"), Problem("B"), Problem("C")))
      case _ ⇒ fail()
    }
    val multiProblem: Problem = Problem.Multiple(List(new Problem.Lazy("A"), new Problem.Lazy("B")))
    multiProblem |+| Problem("C") match {
      case Problem.Multiple(problems) ⇒ assert(problems == List(Problem("A"), Problem("B"), Problem("C")))
      case _ ⇒ fail()
    }
    Problem("X") |+| multiProblem match {
      case Problem.Multiple(problems) ⇒ assert(problems == List(Problem("X"), Problem("A"), Problem("B")))
      case _ ⇒ fail()
    }
  }

  "head" in {
    val a = Problem("A")
    assert(a.head eq a)
    val ma = new Problem.Lazy("A")
    val mb = new Problem.Lazy("B")
    val m = new Problem.Multiple(List(ma, mb))
    assert(m.head eq ma)
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
