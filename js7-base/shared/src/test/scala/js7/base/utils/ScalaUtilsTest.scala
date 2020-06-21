package js7.base.utils

import java.util.concurrent.atomic.AtomicBoolean
import js7.base.exceptions.StandardPublicException
import js7.base.problem.Problems.{DuplicateKey, UnknownKeyProblem}
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils._
import js7.base.utils.ScalaUtils.syntax._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._
import scala.reflect.ClassTag
import scala.util.control.NoStackTrace

final class ScalaUtilsTest extends AnyFreeSpec
{
  "reuseIfEqual" in {
    case class A(number: Int)
    val a = A(1)
    val b = A(1)
    val c = A(2)
    assert(a == b && a.ne(b))
    assert(reuseIfEqual(a, b) eq a)
    assert(reuseIfEqual(b, c) eq c)
    assert(reuseIfEqual(a)(_.copy(number = 7)) ne a)
    assert(reuseIfEqual(a)(_.copy(number = 1)) eq a)
  }

  "implicitClass" in {
    def f[A : ClassTag] = implicitClass[A]
    f[String] shouldEqual classOf[String]
  }

  "scalaName" in {
    assert(ScalaUtilsTest.getClass.getName == "js7.base.utils.ScalaUtilsTest$")
    assert(ScalaUtilsTest.getClass.scalaName == "js7.base.utils.ScalaUtilsTest")
  }

  "scalaSimpleName" in {
    assert(ScalaUtilsTest.getClass.getSimpleName == "ScalaUtilsTest$")
    assert(ScalaUtilsTest.getClass.simpleScalaName == "ScalaUtilsTest")
  }

  "simpleClassName" in {
    object A {
      object B {
        def getSimpleName = getClass.getSimpleName
        def simpleName = getClass.simpleName
      }
    }

    //scala-js does not know Java SourceVersion:
    //if (SourceVersion.values map (_.toString) contains "RELEASE_9")
    //  assert(A.B.getSimpleName == "B$")
    //else
    //  intercept[java.lang.InternalError] {  // Until Java 8: https://bugs.openjdk.java.net/browse/JDK-8057919
    //    A.B.getSimpleName
    //  }
    assert(A.B.simpleName == "B$")
    assert(simpleClassName("C") == "C")
    assert(simpleClassName("a.C") == "C")
    assert(simpleClassName("a.C$") == "C$")
    assert(simpleClassName("aa.bbb.C") == "C")
    assert(simpleClassName("aa.B$C") == "C")
    assert(simpleClassName("aa.B$CC") == "CC")
  }

  "shortClassName" in {
    assert(ScalaUtilsTest.getClass.shortClassName == "ScalaUtilsTest")
    assert((new ScalaUtilsTest.Inner).getClass.shortClassName == "ScalaUtilsTest.Inner")
    class Local
    assert((new Local).getClass.shortClassName == "ScalaUtilsTest$Local.1")
    assert(7.getClass.shortClassName == "Int")
  }

  "Function1.withToString" in {
    def function(o: Int) = 2*o
    val f = function _
    val g = f.withToString("My function")
    g(3) shouldEqual f(3)
    g.toString() shouldEqual "My function"
  }

  "withToString" in {
    val f = function0WithToString("TEST") { 7 }
    assert(f() == 7)
    assert(f.toString == "TEST")
  }

  "function1WithToString" in {
    val f = function1WithToString("TEST") { i: Int => 2 * i }
    assert(f(3) == 6)
    assert(f.toString == "TEST")
  }

  "Throwable" - {
    "rootCause" in {
      new Exception("A", new Exception("B", new Exception("ROOT"))).rootCause.getMessage shouldEqual "ROOT"
    }

    "toStringWithCauses" in {
      assert(new RuntimeException("TEST").toStringWithCauses == "TEST")
      assert(new RuntimeException("TEST", new IllegalStateException("STATE")).toStringWithCauses ==
        "TEST, caused by: IllegalStateException: STATE")
    }

    "toSimplifiedString" in {
      assert(new RuntimeException("ERROR").toSimplifiedString == "ERROR")
      assert(new IllegalArgumentException("ERROR").toSimplifiedString == "ERROR")
      assert(new StandardPublicException("ERROR").toSimplifiedString == "ERROR")
      assert(new IllegalStateException("ERROR").toSimplifiedString == "IllegalStateException: ERROR")
    }

    "nullIfNoStackTrace" in {
      val withStackTrace = new Throwable
      val withoutStackTrace = new Throwable("TEST") with NoStackTrace
      assert(withStackTrace.nullIfNoStackTrace eq withStackTrace)
      assert(withoutStackTrace.nullIfNoStackTrace eq null)
      scribe.debug(s"nullIfNoStackTrace: ${withoutStackTrace.toStringWithCauses}", withoutStackTrace)
    }

    "ifNoStackTrace" in {
      val withStackTrace = new Throwable
      val withoutStackTrace = new Throwable("TEST") with NoStackTrace
      assert(withStackTrace.ifNoStackTrace.get eq withStackTrace)
      assert(withoutStackTrace.ifNoStackTrace == None)
    }
  }

  "cast" in {
    val s: Any = "Hej!"
    val string = cast[String](s)
    (string: String) shouldEqual "Hej!"
    intercept[ClassCastException]{ cast[String](123) } .getMessage shouldEqual "Expected java.lang.Integer but got java.lang.String: 123"
  }

  "checkedCast" in {
    val s: Any = "Hej!"
    val string = checkedCast[String](s)
    string shouldEqual Right("Hej!")
    assert(checkedCast[String](123) == Left(Problem("Expected java.lang.Integer but got java.lang.String: 123")))
    assert(checkedCast[String](null).left.exists(_.throwable.isInstanceOf[NullPointerException]))
  }

  "someUnless" in {
    someUnless(7, none = 0) shouldEqual Some(7)
    someUnless(0, none = 0) shouldEqual None
  }

  "|> (apply function, pipeline operator)" in {
    assert((3 |> (_ * 7)) == 3 * 7)
    def double(a: Int) = 2 * a
    assert((3 |> double) == double(3))
  }

  "pipeIf" in {
    assert(3.pipeIf(true, _ * 7) == 3 * 7)
    assert(3.pipeIf(false, _ * 7) == 3)
  }

  "substitute" in {
    7 substitute 7 -> 3 shouldEqual 3
    7.substitute(7, 3) shouldEqual 3
    7 substitute 4 -> 3 shouldEqual 7
    7.substitute(4, sys.error("ERROR")) shouldEqual 7
    "" substitute "" -> null shouldEqual null
  }

  "AtomicBoolean.switchOn" in {
    val a = new AtomicBoolean
    var x = 0
    a.switchOn { x = 1 }
    assert(x == 1)
    a.switchOn { x = 2 }
    assert(x == 1)
    a.switchOff { x = 3 }
    assert(x == 3)
    a.switchOff { x = 4 }
    assert(x == 3)
  }

  "switch" in {
    var r = 0
    def f(i: Int) =
      i switch {
        case 11 => r = i
      }
    assert(r == 0)
    f(11)
    assert(r == 11)
    f(22)
    assert(r == 11)
  }

  "RichPartialFunction" - {
    val pf: PartialFunction[Int, String] = {
      case 1 => "ONE"
    }

    "PartialFunction.checked" in {
      assert(pf.checked(1) == Right("ONE"))
      assert(pf.checked(2) == Left(UnknownKeyProblem("Int", 2)))
    }

    "PartialFunction.checked as curried function" in {
      val checked: Int => Checked[String] = pf.checked
      assert(checked(1) == Right("ONE"))
      assert(checked(2) == Left(UnknownKeyProblem("Int", 2)))
    }

    "PartialFunction.doNotContain" in {
      assert(pf.checkNoDuplicate(1) == Left(DuplicateKey("Int", 1)))
      assert(pf.checkNoDuplicate(2) == Right(()))
    }

    "PartialFunction.toThrowableChecked" in {
      assert(pf.checked(1) == Right("ONE"))
      assert(pf.checked(2) == Left(UnknownKeyProblem("Int", 2)))
    }

    "PartialFunction.getOrElse" in {
      assert(pf.getOrElse(1, "DEFAULT") == "ONE")
      assert(pf.getOrElse(2, "DEFAULT") == "DEFAULT")
    }

    "PartialFunction.callIfDefined" in {
      var x = 0
      val pf: PartialFunction[Int, Unit] = {
        case 1 => x = 1
      }
      pf.callIfDefined(2)
      assert(x == 0)
      pf.callIfDefined(1)
      assert(x == 1)
    }

    "PartialFunction map" in {
      case class A(string: String)
      val pf: PartialFunction[Int, A] = {
        case 1 => A("one")
      }

      assert(pf(1) == A("one"))
      assert(pf.isDefinedAt(1))
      assert(!pf.isDefinedAt(2))
      assert(pf.applyOrElse(1, (i: Int) => A(s"else $i")) == A("one"))
      assert(pf.applyOrElse(2, (i: Int) => A(s"else $i")) == A("else 2"))

      val mappedPf = pf map (_.string)
      assert(mappedPf(1) == "one")
      assert(mappedPf.isDefinedAt(1))
      assert(!mappedPf.isDefinedAt(2))
      assert(mappedPf.applyOrElse(1, (i: Int) => s"else $i") == "one")
      assert(mappedPf.applyOrElse(2, (i: Int) => s"else $i") == "else 2")
    }
  }

  "Either orElse" in {
    assert((Left(1): Either[Int, Boolean]).orElse(Left("WINNER")) == Left("WINNER"))
    assert((Left(1): Either[Int, Boolean]).orElse(Right("WINNER")) == Right("WINNER"))
    assert((Right(true): Either[Int, Boolean]).orElse(Left("LOOSER")) == Right(true))
    assert((Right(true): Either[Int, Boolean]).orElse(Right("LOOSER")) == Right(true))
  }

  "Either.toFuture" in {
    assert(Right[Throwable, Int](7).toFuture.value.get.get == 7)
    val t = new IllegalArgumentException
    assert(Left[Throwable, Int](t).toFuture.failed.value.get.get eq t)
  }

  "Either.toThrowableChecked" in {
    assert(Right[Throwable, Int](7).toThrowableChecked == Right(7))
    val t = new IllegalArgumentException
    assert(Left[Throwable, Int](t).toThrowableChecked.swap.getOrElse(null).throwable eq t)
  }

  "Either.toMessageOnlyChecked" in {
    assert(Right[Throwable, Int](7).toMessageOnlyChecked == Right(7))
    val t = new IllegalArgumentException("EXCEPTION")
    assert(Left[Throwable, Int](t).toMessageOnlyChecked.swap.getOrElse(null).throwable ne t)
    assert(Left[Throwable, Int](t).toMessageOnlyChecked.swap.getOrElse(null).toString == "EXCEPTION")
  }

  "Either.orThrow" in {
    assert(Right[Throwable, Int](7).orThrow == 7)
    val t = new IllegalArgumentException
    intercept[IllegalArgumentException] {
      (Left[Throwable, Int](t): Either[Throwable, Int]).orThrow
    } should be theSameInstanceAs (t)
  }

  "Either.withStackTrace" - {
    "with stacktrace provided" in {
      assert(Right[Throwable, Int](7).withStackTrace == Right[Throwable, Int](7))
      val t = new IllegalArgumentException
      assert(t.getStackTrace.nonEmpty)
      val Left(t2) = Left[Throwable, Int](t).withStackTrace
      assert(t2 eq t)
    }

    "without stacktrace provided" in {
      val u = new IllegalArgumentException with NoStackTrace
      assert(u.getStackTrace.isEmpty)
      (Left[Throwable, Int](u).withStackTrace: @unchecked) match {
        case Left(uu: IllegalStateException) => assert(uu.getStackTrace.nonEmpty)
      }
    }
  }

  "Boolean.option" in {
    assert((true option 7: Option[Int]) == Some(7))
    assert((false option 7: Option[Int]) == None)
  }

  "Boolean.thenList" in {
    assert((true thenList 7: List[Int]) == List(7))
    assert((false thenList 7: List[Int]) == Nil)
  }

  "Boolean.thenVector" in {
    assert((true thenVector 7: Vector[Int]) == Vector(7))
    assert((false thenVector 7: Vector[Int]) == Vector())
  }

  "Boolean.thenSet" in {
    assert((true thenSet 7: Set[Int]) == Set(7))
    assert((false thenSet 7: Set[Int]) == Set())
  }

  "shortStringToInputStream" in {
    val in = shortStringToInputStream("heiß")
    assert(in.read() == 'h'.toByte)
    assert(in.read() == 'e'.toByte)
    assert(in.read() == 'i'.toByte)
    assert(in.read() == 0xC3)
    assert(in.read() == 0x9F)
    assert(in.read() == -1)
  }

  "truncateWithEllipsis" in {
    assert("".truncateWithEllipsis(0) == "")
    assert("".truncateWithEllipsis(1) == "")
    assert("".truncateWithEllipsis(4) == "")
    assert("A".truncateWithEllipsis(0) == "A")
    assert("AB".truncateWithEllipsis(0) == "AB")
    assert("ABC".truncateWithEllipsis(0) == "ABC")
    assert("ABCD".truncateWithEllipsis(0) == "...")
    assert("ABCDE".truncateWithEllipsis(0) == "...")
    assert("A".truncateWithEllipsis(1) == "A")
    assert("AB".truncateWithEllipsis(1) == "AB")
    assert("ABC".truncateWithEllipsis(1) == "ABC")
    assert("ABCD".truncateWithEllipsis(1) == "...")
    assert("ABCDE".truncateWithEllipsis(1) == "...")
    assert("A".truncateWithEllipsis(2) == "A")
    assert("AB".truncateWithEllipsis(2) == "AB")
    assert("ABC".truncateWithEllipsis(2) == "ABC")
    assert("ABCD".truncateWithEllipsis(2) == "...")
    assert("ABCDE".truncateWithEllipsis(2) == "...")
    assert("A".truncateWithEllipsis(3) == "A")
    assert("AB".truncateWithEllipsis(3) == "AB")
    assert("ABC".truncateWithEllipsis(3) == "ABC")
    assert("ABCD".truncateWithEllipsis(3) == "...")
    assert("ABCDE".truncateWithEllipsis(3) == "...")
    assert("A".truncateWithEllipsis(4) == "A")
    assert("AB".truncateWithEllipsis(4) == "AB")
    assert("ABC".truncateWithEllipsis(4) == "ABC")
    assert("ABCD".truncateWithEllipsis(4) == "ABCD")
    assert("ABCDE".truncateWithEllipsis(4) == "A...")
    assert("A".truncateWithEllipsis(5) == "A")
    assert("AB".truncateWithEllipsis(5) == "AB")
    assert("ABC".truncateWithEllipsis(5) == "ABC")
    assert("ABCD".truncateWithEllipsis(5) == "ABCD")
    assert("ABCDE".truncateWithEllipsis(5) == "ABCDE")
    assert("ABCDE".truncateWithEllipsis(6) == "ABCDE")
    assert("ABCDEF".truncateWithEllipsis(6) == "ABCDEF")
    assert("ABCDEFG".truncateWithEllipsis(6) == "ABC...")
    assert("ABCDEFGHIJKLMNOPQRSTUVWXYZ".truncateWithEllipsis(6) == "ABC...")
    val expected = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx...(length 100)"
    assert(("x" * 100).truncateWithEllipsis(50, showLength = true) == expected)
    assert(expected.length == 50)
  }

  "replaceChar" in {
    val empty = ""
    assert(empty.replaceChar('-', '+') eq empty)
    val a = "abc"
    assert(a.replaceChar('-', '+') eq a)
    assert(("a-b--c---").replaceChar('-', '+') == "a+b++c+++")
  }

  "reverseDropWhile" in {
    assert("".reverseDropWhile(_ == '/') == "")
    assert("/".reverseDropWhile(_ == '/') == "")
    assert("//".reverseDropWhile(_ == '/') == "")
    assert("/abc".reverseDropWhile(_ == '/') == "/abc")
    assert("/abc//".reverseDropWhile(_ == '/') == "/abc")
  }

  "String ?" in {
    assert((false ?: "STRING") == "")
    assert((true ?: "STRING") == "STRING")
    assert(("STRING" when false) == "")
    assert(("STRING" when true) == "STRING")
  }
}

object ScalaUtilsTest
{
  private final class Inner
}
