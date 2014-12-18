package com.sos.scheduler.engine.common.scalautil

import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class CloseOnErrorTest extends FreeSpec {

  "closeOnError" in {
    class A extends CloseOnError {
      var closed = false
      def f(error: Boolean): Unit = {
        closeOnError {
          if (error) sys.error("ERROR")
        }
      }
      override def close(): Unit = {
        closed = true
      }
    }
    val a = new A
    a.f(error = false)
    a.closed shouldBe false
    intercept[Exception] { a.f(error = true) }
    a.closed shouldBe true
  }
}
