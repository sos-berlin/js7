package com.sos.scheduler.engine.common.scalautil

import com.sos.scheduler.engine.common.scalautil.AutoClosing._
import com.sos.scheduler.engine.common.scalautil.AutoClosingTest._
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
final class AutoClosingTest extends FreeSpec {

  "autoClosing without Exception" in {
    val a = new A
    autoClosing(a) { _ => }
    a shouldBe 'closed
  }

  "autoClosing with Exception" in {
    val a = new A
    intercept[Exception] { autoClosing(a) { _ => throw new Exception } } .getSuppressed shouldBe empty
    a shouldBe 'closed
  }

  "autoClosing with second Exception in close" in {
    class B {
      def close(): Unit = {
        throw new Exception("suppressed")
      }
    }
    val x = intercept[Exception] { autoClosing(new B) { _ => throw new Exception("1") } }
    x.getMessage shouldEqual "1"
    x.getSuppressed map { _.getMessage } shouldEqual Array("suppressed")
  }
}

private object AutoClosingTest {
  private final class A {
    var closed = false

    def close(): Unit = {
      require(!closed)
      closed = true
    }
  }
}
