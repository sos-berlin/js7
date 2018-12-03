package com.sos.jobscheduler.base.problem

import cats.syntax.semigroup._
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import io.circe.Decoder
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ProblemTest extends FreeSpec
{
  private implicit val codeToString: ProblemCode ⇒ Option[String] = _  match {
    case ProblemCode("PROBLEM-CODE") ⇒ Some("PROBLEM MESSAGE")
    case ProblemCode("PROBLEM-CODE-1") ⇒ Some("PROBLEM MESSAGE-1 first=$1")
    case ProblemCode("PROBLEM-CODE-1-3") ⇒ Some("PROBLEM MESSAGE-1-3 first=$1 third=$3")
    case _ ⇒ None
  }

  "JSON" - {
    "standard" in {
      testJson(Problem("A problem"),
        json"""{
          "message": "A problem"
        }""")
    }

    "with TYPE" in {
      testJson(Problem("A problem"),
        json"""{
          "TYPE": "Problem",
          "message": "A problem"
        }""")(Problem.typedJsonEncoder, implicitly[Decoder[Problem]])
    }

    "with ProblemCode" in {
      testJson(Problem(ProblemCode("PROBLEM-CODE")),
        json"""{
          "TYPE": "Problem",
          "code": "PROBLEM-CODE",
          "message": "PROBLEM MESSAGE"
        }""")(Problem.typedJsonEncoder, implicitly[Decoder[Problem]])
    }
  }

  "ProblemCode" - {
    "without insertions" in {
      val problem = Problem(ProblemCode("PROBLEM-CODE"))
      assert(problem.toString == "PROBLEM MESSAGE")
    }

    "with insertions" in {
      assert(Problem(ProblemCode("PROBLEM-CODE-1"), "FIRST")
        .toString  == "PROBLEM MESSAGE-1 first=FIRST")
    }

    "with to much insertions" in {
      val problem = Problem(ProblemCode("PROBLEM-CODE-1-3"), "FIRST", "SECOND", "THIRD", "FORTH")
      assert(problem.toString == "PROBLEM MESSAGE-1-3 first=FIRST third=THIRD [SECOND, FORTH]")
    }

    "with missing insertions" in {
      assert(Problem(ProblemCode("PROBLEM-CODE-1-3"), "FIRST")
        .toString  == "PROBLEM MESSAGE-1-3 first=FIRST third=[]")
    }
  }

  "String" in {
    assert(Problem("").toString == "A problem occurred (no message)")
    assert(Problem(null: String).toString == "A problem occurred (null)")

    val problem = Problem("MESSAGE")
    assert(problem.toString == "MESSAGE")
    assert(problem.throwableOption.isEmpty)
    assert(problem.throwable.getMessage == "MESSAGE")
    assert(problem.throwable.asInstanceOf[ProblemException].problem eq problem)

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

  "cause" in {
    assert(new Problem.Lazy("A", Some(Problem("B"))).toString == "A [B]")
    assert(catch_(new Problem.Lazy("A", Some(Problem("B")))) == "A [B]")
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

    assert(catch_(Problem("A") |+| Problem("B")) == "A\n & B")
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
    Problem((throw new Exception): String)
    Problem((throw new Exception): String).withKey("KEY")
    intercept[Exception] {
      Problem((throw new Exception): String).toString
    }
  }

  "throwable" in {
    val throwable = intercept[ProblemException] {
      throw Problem("PROBLEM").throwable
    }
    assert(throwable.toString == "ProblemException: PROBLEM")
  }

  "throwable withPrefix" in {
    val throwable = intercept[ProblemException] {
      throw Problem("PROBLEM").withPrefix("PREFIX:").throwable
    }
    assert(throwable.toString == "ProblemException: PREFIX: PROBLEM")
  }

  "Problem.eager" in {
    assert(Problem.eager(new RuntimeException("EXCEPTION")).toString == "EXCEPTION")
    assert(Problem.eager(new RuntimeException("EXCEPTION")).withPrefix("PREFIX").toString == "PREFIX, caused by: EXCEPTION")
  }

  "equals" in {
    assert(Problem(ProblemCode("A")) == Problem(ProblemCode("A")))
    assert(Problem(ProblemCode("A"), 1) == Problem(ProblemCode("A"), 1))
    assert(Problem(ProblemCode("A"), 1) != Problem(ProblemCode("A"), 2))
    assert(Problem(ProblemCode("B")) != Problem(ProblemCode("A")))
    assert(Problem("TEST") == Problem("TEST"))
    assert(Problem("TEST").withPrefix("PREFIX") == Problem("PREFIX\n & TEST"))
    assert(Problem("TEST").withPrefix("PREFIX:") == Problem("PREFIX: TEST"))
    assert(Problem("TEST").wrapProblemWith("WRAP") == Problem("WRAP [TEST]"))
    assert(Problem("X") != Problem("Y"))
  }

  private def catch_(problem: Problem): String =
    intercept[ProblemException] {
      throw problem.throwable
    }.toStringWithCauses
}
