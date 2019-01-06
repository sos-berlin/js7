package com.sos.jobscheduler.base.utils

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.exceptions.StandardPublicException
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.utils.ScalaUtils._
import com.sos.jobscheduler.base.utils.ScalaUtils.implicits._
import java.util.concurrent.atomic.AtomicBoolean
import javax.lang.model.SourceVersion
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import scala.reflect.ClassTag
import scala.util.control.NoStackTrace

final class ScalaUtilsTest extends FreeSpec {

  "implicitClass" in {
    def f[A : ClassTag] = implicitClass[A]
    f[String] shouldEqual classOf[String]
  }

  "scalaName" in {
    assert(ScalaUtilsTest.getClass.getName == "com.sos.jobscheduler.base.utils.ScalaUtilsTest$")
    assert(ScalaUtilsTest.getClass.scalaName == "com.sos.jobscheduler.base.utils.ScalaUtilsTest")
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

    if (SourceVersion.values map (_.toString) contains "RELEASE_9")
      assert(A.B.getSimpleName == "B$")
    else
      intercept[java.lang.InternalError] {  // Until Java 8: https://bugs.openjdk.java.net/browse/JDK-8057919
        A.B.getSimpleName
      }
    assert(A.B.simpleName == "B$")
    assert(simpleClassName("C") == "C")
    assert(simpleClassName("a.C") == "C")
    assert(simpleClassName("a.C$") == "C$")
    assert(simpleClassName("aa.bbb.C") == "C")
    assert(simpleClassName("aa.B$C") == "C")
    assert(simpleClassName("aa.B$CC") == "CC")
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
    val f = function1WithToString("TEST") { i: Int ⇒ 2 * i }
    assert(f(3) == 6)
    assert(f.toString == "TEST")
  }

  "Throwable.rootCause" in {
    new Exception("A", new Exception("B", new Exception("ROOT"))).rootCause.getMessage shouldEqual "ROOT"
  }

  "Throwable.toStringWithCauses" in {
    assert(new RuntimeException("TEST").toStringWithCauses == "TEST")
    assert(new RuntimeException("TEST", new IllegalStateException("STATE")).toStringWithCauses ==
      "TEST, caused by: IllegalStateException: STATE")
  }

  "Throwable.toSimplifiedString" in {
    assert(new RuntimeException("ERROR").toSimplifiedString == "ERROR")
    assert(new IllegalArgumentException("ERROR").toSimplifiedString == "ERROR")
    assert(new StandardPublicException("ERROR").toSimplifiedString == "ERROR")
    assert(new IllegalStateException("ERROR").toSimplifiedString == "IllegalStateException: ERROR")
  }

  "cast" in {
    val s: Any = "Hej!"
    val string = cast[String](s)
    (string: String) shouldEqual "Hej!"
    intercept[ClassCastException]{ cast[String](123) } .getMessage shouldEqual "java.lang.Integer is not a java.lang.String: 123"
  }

  "someUnless" in {
    someUnless(7, none = 0) shouldEqual Some(7)
    someUnless(0, none = 0) shouldEqual None
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
        case 11 ⇒ r = i
      }
    assert(r == 0)
    f(11)
    assert(r == 11)
    f(22)
    assert(r == 11)
  }

  "PartialFunction.checked" in {
    val pf: PartialFunction[Int, String] = {
      case 1 ⇒ "1"
    }
    assert(pf.checked(1) == Valid("1"))
    assert(pf.checked(2) == Invalid(Problem("No such key '2'")))
  }

  "PartialFunction.toChecked" in {
    val pf: PartialFunction[Int, String] = {
      case 1 ⇒ "1"
    }
    assert(pf.toChecked(1) == Valid("1"))
    assert(pf.toChecked(2) == Invalid(Problem("No such key '2'")))
  }

  "PartialFunction.getOrElse" in {
    val pf: PartialFunction[Int, String] = {
      case 1 ⇒ "1"
    }
    assert(pf.getOrElse(1, "1") == "1")
    assert(pf.getOrElse(2, "-") == "-")
  }

  "PartialFunction.callIfDefined" in {
    var x = 0
    val pf: PartialFunction[Int, Unit] = {
      case 1 ⇒ x = 1
    }
    pf.callIfDefined(2)
    assert(x == 0)
    pf.callIfDefined(1)
    assert(x == 1)
  }

  "PartialFunction map" in {
    case class A(string: String)
    val pf: PartialFunction[Int, A] = {
      case 1 ⇒ A("one")
    }

    assert(pf(1) == A("one"))
    assert(pf.isDefinedAt(1))
    assert(!pf.isDefinedAt(2))
    assert(pf.applyOrElse(1, (i: Int) ⇒ A(s"else $i")) == A("one"))
    assert(pf.applyOrElse(2, (i: Int) ⇒ A(s"else $i")) == A("else 2"))

    val mappedPf = pf map (_.string)
    assert(mappedPf(1) == "one")
    assert(mappedPf.isDefinedAt(1))
    assert(!mappedPf.isDefinedAt(2))
    assert(mappedPf.applyOrElse(1, (i: Int) ⇒ s"else $i") == "one")
    assert(mappedPf.applyOrElse(2, (i: Int) ⇒ s"else $i") == "else 2")
  }

  "Either.toImmediateFuture" in {
    assert(Right[Throwable, Int](7).toImmediateFuture.value.get.get == 7)
    val t = new IllegalArgumentException
    assert(Left[Throwable, Int](t).toImmediateFuture.failed.value.get.get eq t)
  }

  "Either.toChecked" in {
    assert(Right[Throwable, Int](7).toChecked == Valid(7))
    val t = new IllegalArgumentException
    assert(Left[Throwable, Int](t).toChecked.swap.getOrElse(null).throwable eq t)
  }

  "Either.toSimpleChecked" in {
    assert(Right[Throwable, Int](7).toSimpleChecked == Valid(7))
    val t = new IllegalArgumentException("EXCEPTION")
    assert(Left[Throwable, Int](t).toSimpleChecked.swap.getOrElse(null).throwable ne t)
    assert(Left[Throwable, Int](t).toSimpleChecked.swap.getOrElse(null).toString == "EXCEPTION")
  }

  "Either.orThrow" in {
    assert(Right[Throwable, Int](7).orThrow == 7)
    val t = new IllegalArgumentException
    intercept[IllegalArgumentException] {
      Left[Throwable, Int](t).orThrow
    } should be theSameInstanceAs (t)
  }

  "Either.withStackTrace" - {
    "with stacktrace provided" in {
      assert(Right[Throwable, Int](7).withStackTrace == Right[Throwable, Int](7))
      val t = new IllegalArgumentException
      assert(t.getStackTrace.nonEmpty)
      assert(Left[Throwable, Int](t).withStackTrace.left.get eq t)
    }

    "without stacktrace provided" in {
      val u = new IllegalArgumentException with NoStackTrace
      assert(u.getStackTrace.isEmpty)
      Left[Throwable, Int](u).withStackTrace.left.get match {
        case uu: IllegalStateException => assert(uu.getStackTrace.nonEmpty)
      }
    }
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
}

object ScalaUtilsTest
