package js7.base.problem

import cats.syntax.semigroup._
import io.circe.Decoder
import js7.base.circeutils.CirceUtils._
import js7.base.problem.ProblemTest._
import js7.base.utils.ScalaUtils.syntax._
import js7.tester.CirceJsonTester.testJson
import org.scalatest.exceptions.TestFailedException
import org.scalatest.freespec.AnyFreeSpec
import scala.util.Try

/**
  * @author Joacim Zschimmer
  */
final class ProblemTest extends AnyFreeSpec
{
  "JSON" - {
    "without ProblemCode" - {
      "standard" in {
        testJson(Problem("A problem"),
          json"""{
            "message": "A problem"
          }""")
      }

      "with TYPE" in {
        implicit val x = Problem.typedJsonEncoder
        implicit val y = implicitly[Decoder[Problem]]
        testJson(Problem("A problem"),
          json"""{
            "TYPE": "Problem",
            "message": "A problem"
          }""")
      }
    }

    "with ProblemCode" in {
      val problem = TestCodeProblem(Map("argument" -> "ARGUMENT"))
      val message = problem.messageWithCause  // While testing, ProblemCodeMessages.initialize() may be called or not, so exact message depends
      testJson[Problem](TestCodeProblem(Map("argument" -> "ARGUMENT")),
        json"""{
          "code": "TestCode",
          "arguments": {
            "argument": "ARGUMENT"
          },
          "message": "$message"
        }""")
      assert(problem == Problem.HasCode(ProblemCode("TestCode"), Map("argument" -> "ARGUMENT")))
      assert(problem != Problem.HasCode(ProblemCode("TestCode"), Map("argument" -> "X")))
      assert(problem != Problem.HasCode(ProblemCode("XXXXXXXX"), Map("argument" -> "ARGUMENT")))
    }
  }

  "String" in {
    val a = TestCodeProblem(Map("argument" -> "ARGUMENT")).toString
    assert(TestCodeProblem(Map("argument" -> "ARGUMENT")).toString == "TestCode(argument=ARGUMENT)")

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

    assert(problem.withKey("KEY").toString == "Problem with 'KEY': MESSAGE")
    assert(problem.withKey("KEY").throwable.getMessage == "Problem with 'KEY': MESSAGE")

    assert(Problem.fromLazyThrowable(new RuntimeException).toString == "java.lang.RuntimeException")
  }

  "cause" in {
    assert(Problem("A", Some(Problem("B"))).toString == "A [B]")
    assert(Problem.pure("A", Some(Problem("B"))).toString == "A [B]")
    assert(new Problem.Lazy("A", Some(Problem("B"))).toString == "A [B]")
    assert(catch_(new Problem.Lazy("A", Some(Problem("B")))) == "A [B]")
  }

  "combine" in {
    assert((Problem("A") |+| Problem("B")) == Problem("A;\nB"))
    assert((Problem("A:") |+| Problem("B")) == Problem("A: B"))
    assert((Problem("A: ") |+| Problem("B")) == Problem("A: B"))
    assert((Problem("A -") |+| Problem("B")) == Problem("A - B"))
    assert((Problem("A - ") |+| Problem("B")) == Problem("A - B"))
    assert((Problem("A") |+| Problem.fromLazyThrowable(new RuntimeException("B"))).throwableOption.get.getMessage == "A;\nB")
    assert((Problem("A") |+| Problem.fromLazyThrowable(new RuntimeException("B"))).throwableOption.get.toStringWithCauses == "A;\nB")
    assert((Problem.fromLazyThrowable(new RuntimeException("A")) |+| Problem("B")).throwableOption.get.getMessage == "A;\nB")
    assert((Problem.fromLazyThrowable(new RuntimeException("A")) |+| Problem("B")).throwableOption.get.toStringWithCauses == "A;\nB")
    assert((Problem.fromLazyThrowable(new RuntimeException("A")) |+| Problem.fromLazyThrowable(new RuntimeException("B"))).throwableOption.get.getMessage == "A;\nB")
    assert((Problem.fromLazyThrowable(new RuntimeException("A")) |+| Problem.fromLazyThrowable(new RuntimeException("B"))).throwableOption.get.toStringWithCauses == "A;\nB")

    assert(catch_(Problem("A") |+| Problem("B")) == "A;\nB")
  }

  "Combined" in {
    Problem("A") |+| Problem("B") match {
      case Problem.Combined(problems) => assert(problems == List(Problem("A"), Problem("B")))
      case _ => fail()
    }
  }

  "Combined shows only distinct problems" in {
    assert((Problem("X") |+| Problem("X")).toString == "X")
  }

  "Combined is flat" in {
    Problem("A") |+| Problem("B") |+| Problem("C") match {
      case Problem.Combined(problems) => assert(problems == List(Problem("A"), Problem("B"), Problem("C")))
      case _ => fail()
    }
    val multiProblem: Problem = Problem.Combined(List(new Problem.Lazy("A"), new Problem.Lazy("B")))
    multiProblem |+| Problem("C") match {
      case Problem.Combined(problems) => assert(problems == List(Problem("A"), Problem("B"), Problem("C")))
      case _ => fail()
    }
    Problem("X") |+| multiProblem match {
      case Problem.Combined(problems) => assert(problems == List(Problem("X"), Problem("A"), Problem("B")))
      case _ => fail()
    }
  }

  "Combined combines all optional stacktraces" in {
    def throwB() = throw new RuntimeException("B-EXCEPTION")
    def throwD() = throw new RuntimeException("D-EXCEPTION")
    val a = Problem("A-PROBLEM")
    val b = Problem.fromThrowable(Try(throwB()).failed.get: Throwable)
    val c = Problem("C-PROBLEM")
    val d = Problem.fromThrowable(Try(throwD()).failed.get: Throwable)
    val combinedThrowable = (a |+| b |+| c |+| d).throwable
    assert(combinedThrowable.toString == "ProblemException: A-PROBLEM;\nB-EXCEPTION;\nC-PROBLEM;\nD-EXCEPTION")
    assert(combinedThrowable.getStackTrace.exists(_.getMethodName contains "throwB"))
    assert(combinedThrowable.getStackTrace.exists(_.getMethodName contains "throwD"))
  }

  "head" in {
    val a = Problem("A")
    assert(a.head eq a)
    val ma = new Problem.Lazy("A")
    val mb = new Problem.Lazy("B")
    val m = Problem.Combined(List(ma, mb))
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

  "Problem.fromThrowable" in {
    assert(Problem.fromThrowable(new RuntimeException("EXCEPTION")).toString == "EXCEPTION")
    assert(Problem.fromThrowable(new RuntimeException("EXCEPTION")).withPrefix("PREFIX:").toString == "PREFIX: EXCEPTION")
  }

  "equals" in {
    assert(TestCodeProblem(Map.empty) == TestCodeProblem(Map.empty))
    assert((TestCodeProblem(Map("a" -> "A")): Problem) != TestProblem(a = "A"))
    assert(TestCodeProblem(Map("a" -> "A")) == TestCodeProblem(Map("a" -> "A")))
    assert(TestCodeProblem(Map("a" -> "A")) != TestCodeProblem(Map("a" -> "X")))
    assert(Problem("TEST") == Problem("TEST"))
    assert(Problem("TEST").withPrefix("PREFIX") == Problem("PREFIX;\nTEST"))
    assert(Problem("TEST").withPrefix("PREFIX:") == Problem("PREFIX: TEST"))
    assert(Problem("TEST").wrapProblemWith("WRAP") == Problem("WRAP [TEST]"))
    assert(Problem("X") != Problem("Y"))
  }

  "equals after serialization" in {
    locally {
      val a: Problem = Problem.HasCodeAndMessage(TestCodeProblem.code, Map("a" -> "A"), "MSG")
      val b = TestCodeProblem(Map("a" -> "A"))
      assert(a == b)
      assert(b == a)
    }
    locally {
      val a: Problem = Problem.HasCodeAndMessage(TestProblem.code, Map("a" -> "A"), "MSG")
      val b = TestProblem(a = "A")
      assert(a == b)
      assert(b == a)
    }
  }

  "unapply after serialization does not work" in {
    intercept[TestFailedException] {
      val a: Problem = Problem.HasCodeAndMessage(TestProblem.code, Map("a" -> "A"), "MSG")
      a match {
        case TestProblem(a) => assert(a == "A")
        case _ => fail("unapply after serialization fails")
      }
    }
  }

  "is" in {
    assert(!Problem.pure("PROBLEM").is(TestCodeProblem))
    assert(Problem.HasCode(ProblemCode("TestCode"), Map("x" -> "y")).is(TestCodeProblem))
    assert(TestCodeProblem(Map("x" -> "y")) is TestCodeProblem)
    assert(!Problem.HasCode(ProblemCode("X"), Map.empty).is(TestCodeProblem))
  }

  "Problem.HasCode.unapply" in {
    val problem = new Problem.HasCode {
      val code = ProblemCode("PROBLEM")
      val arguments = Map("ARG" -> "VALUE")
    }
    val NoArguments = Map.empty[String, String]
    problem match {
      case Problem.HasCode(ProblemCode("OTHER"), problem.arguments) => fail()
      case Problem.HasCode(ProblemCode("PROBLEM"), NoArguments) => fail()
      case Problem.HasCode(ProblemCode("PROBLEM"), problem.arguments) =>  // okay
    }
  }

  "Problem.IsThrowable" in {
    Problem("PROBLEM") match {
      case Problem.IsThrowable(_: IllegalStateException) => fail()
      case _ =>
    }
    Problem.fromLazyThrowable(new IllegalStateException("TEST")) match {
      case Problem.IsThrowable(_: IllegalStateException) =>
      case _ => fail()
    }
    Problem.fromLazyThrowable(new IllegalArgumentException("TEST")) match {
      case Problem.IsThrowable(_: IllegalStateException) => fail()
      case _ =>
    }
  }

  private def catch_(problem: Problem): String =
    intercept[ProblemException] { throw problem.throwable } .toStringWithCauses
}

object ProblemTest {
  final case class TestProblem(a: String) extends Problem.Coded {
    def arguments = Map("a" -> a)
  }
  object TestProblem extends Problem.Coded.Companion
}
