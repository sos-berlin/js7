package com.sos.scheduler.engine.common.scalautil

import com.google.common.io.Closer
import com.sos.scheduler.engine.common.scalautil.AutoClosing._
import com.sos.scheduler.engine.common.scalautil.AutoClosingTest._
import java.io.Closeable
import org.junit.runner.RunWith
import org.mockito.Mockito._
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import org.scalatest.mock.MockitoSugar.mock

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
      def close() = throw new Exception("suppressed")
    }
    val x = intercept[Exception] { autoClosing(new B) { _ => throw new Exception("1") } }
    x.getMessage shouldEqual "1"
    x.getSuppressed map { _.getMessage } shouldEqual Array("suppressed")
  }

  "closeOnError" in {
    val closer = Closer.create()
    val a = mock[Closeable]
    closer.register(a)
    closeOnError(closer) {}
    verify(a, never).close()
    intercept[IllegalStateException] { closeOnError(closer) { throw new IllegalStateException } }
    verify(a, times(1)).close()
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
