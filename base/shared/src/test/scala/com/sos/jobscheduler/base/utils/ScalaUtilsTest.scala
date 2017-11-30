package com.sos.jobscheduler.base.utils

import com.sos.jobscheduler.base.exceptions.StandardPublicException
import com.sos.jobscheduler.base.utils.ScalaUtils._
import com.sos.jobscheduler.base.utils.ScalaUtils.implicits._
import java.util.concurrent.atomic.AtomicBoolean
import javax.lang.model.SourceVersion
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import scala.concurrent.Future
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
    val f = functionWithToString("TEST") { 7 }
    assert(f() == 7)
    assert(f.toString == "TEST")
  }

  "withToString1" in {
    val f = withToString1("TEST") { i: Int ⇒ 2 * i }
    assert(f(3) == 6)
    assert(f.toString == "TEST")
  }

  "Throwable.rootCause" in {
    new Exception("A", new Exception("B", new Exception("ROOT"))).rootCause.getMessage shouldEqual "ROOT"
  }

  "Throwable.toStringWithCauses" in {
    assert(new RuntimeException("TEST").toStringWithCauses == "TEST")
    assert(new RuntimeException("TEST", new IllegalStateException("STATE")).toStringWithCauses ==
      "TEST, caused by: java.lang.IllegalStateException: STATE")
  }

  "Throwable.toSimplifiedString" in {
    assert(new RuntimeException("ERROR").toSimplifiedString == "ERROR")
    assert(new IllegalArgumentException("ERROR").toSimplifiedString == "ERROR")
    assert(new StandardPublicException("ERROR").toSimplifiedString == "ERROR")
    assert(new IllegalStateException("ERROR").toSimplifiedString == "java.lang.IllegalStateException: ERROR")
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

  "Either.toImmediateFuture" in {
    assert(Right[Throwable, Int](7).toImmediateFuture.value.get.get == 7)
    val t = new IllegalArgumentException
    assert(Left[Throwable, Int](t).toImmediateFuture.failed.value.get.get eq t)
  }

  "Either.force" in {
    assert(Right[Throwable, Int](7).force == 7)
    val t = new IllegalArgumentException
    intercept[IllegalArgumentException] {
      Left[Throwable, Int](t).force
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
}

object ScalaUtilsTest
