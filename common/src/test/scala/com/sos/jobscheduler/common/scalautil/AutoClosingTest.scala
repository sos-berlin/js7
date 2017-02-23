package com.sos.scheduler.engine.common.scalautil

import com.google.common.io.Closer
import com.sos.scheduler.engine.common.scalautil.AutoClosing._
import com.sos.scheduler.engine.common.scalautil.AutoClosingTest._
import java.io.Closeable
import org.mockito.Mockito._
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.mock.MockitoSugar.mock

final class AutoClosingTest extends FreeSpec {

  "autoClosing" - {
    "without Exception" in {
      val a = new A
      autoClosing(a) { _ ⇒ }
      a shouldBe 'closed
    }

    "with Exception" in {
      val a = new A
      intercept[AException] {
        autoClosing(a) { _ ⇒
          throw new AException
        }
      } .getSuppressed shouldBe empty
      a shouldBe 'closed
    }

    "with second Exception in close" in {
      val x = intercept[AException] {
        autoClosing(new FailingClose) { _ ⇒
          throw new AException
        }
      }
      for (suppressed ← x.getSuppressed) suppressed.asInstanceOf[ClosedException]
    }
  }

  "multipleAutoClosing" - {
    class AutoCloseableA extends A with AutoCloseable

    "without Exception" in {
      val closeables = List(new AutoCloseableA, new AutoCloseableA, new AutoCloseableA)
      multipleAutoClosing(closeables) { o ⇒
        assert(o eq closeables)
      }
      assert(closeables forall { _.closed })
    }

    "with Exception" in {
      val closeables = List(new AutoCloseableA, new AutoCloseableA, new AutoCloseableA)
      intercept[Exception] {
        multipleAutoClosing(closeables) { _ ⇒
          throw new Exception
        }
      } .getSuppressed shouldBe empty
      assert(closeables forall { _.closed })
    }

    "with second Exception in close" in {
      val closeables = List(new AutoCloseableA, new FailingClose, new AutoCloseableA)
      val x = intercept[AException] {
        multipleAutoClosing(closeables) { _ ⇒
          throw new AException
        }
      }
      assert(closeables collect { case a: A ⇒ a } forall { _.closed })
      for (suppressed ← x.getSuppressed) {
        suppressed.asInstanceOf[AssertionError]  // Guava Closer wraps a checked exception into an AssertionError
          .getCause.asInstanceOf[ClosedException]
      }
    }
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
  private class A {
    var closed = false

    def close(): Unit = {
      require(!closed)
      closed = true
    }
  }

  private final class FailingClose extends AutoCloseable {
    def close() = throw new ClosedException
  }

  private final class AException extends Exception
  private final class ClosedException extends Exception("CLOSE ERROR")
}
