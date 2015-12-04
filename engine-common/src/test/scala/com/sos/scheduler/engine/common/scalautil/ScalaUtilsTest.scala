package com.sos.scheduler.engine.common.scalautil

import com.sos.scheduler.engine.common.scalautil.ScalaUtils._
import com.sos.scheduler.engine.common.scalautil.ScalaUtils.implicits._
import java.util.concurrent.atomic.AtomicBoolean
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import scala.reflect.ClassTag

@RunWith(classOf[JUnitRunner])
final class ScalaUtilsTest extends FreeSpec {

  "implicitClass" in {
    def f[A : ClassTag] = implicitClass[A]
    f[String] shouldEqual classOf[String]
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

  "cast" in {
    val s: Any = "Hej!"
    val string = cast[String](s)
    (string: String) shouldEqual "Hej!"
    intercept[ClassCastException]{ cast[String](123) } .getMessage shouldEqual "'123': java.lang.Integer is not a java.lang.String"
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
}
