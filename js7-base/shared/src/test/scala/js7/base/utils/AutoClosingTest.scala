package js7.base.utils

import js7.base.test.OurTestSuite
import js7.base.utils.AutoClosing.*
import js7.base.utils.AutoClosingTest.*
import org.scalatest.matchers.should.Matchers.*

final class AutoClosingTest extends OurTestSuite:

  "autoClosing" - {
    "without Exception" in:
      val a = new A
      autoClosing(a) { _ => }
      a shouldBe Symbol("closed")

    "with Exception" in:
      val a = new A
      intercept[AException] {
        autoClosing(a) { _ =>
          throw new AException
        }
      } .getSuppressed shouldBe empty
      a shouldBe Symbol("closed")

    "with second Exception in close" in:
      val x = intercept[AException]:
        autoClosing(new FailingClose) { _ =>
          throw new AException
        }
      for suppressed <- x.getSuppressed do suppressed.asInstanceOf[ClosedException]
  }

  "multipleAutoClosing" - {
    class AutoCloseableA extends A, AutoCloseable

    "without Exception" in:
      val closeables = List(new AutoCloseableA, new AutoCloseableA, new AutoCloseableA)
      multipleAutoClosing(closeables) { o =>
        assert(o eq closeables)
      }
      assert(closeables.forall(_.closed))

    "with Exception" in:
      val closeables = List(new AutoCloseableA, new AutoCloseableA, new AutoCloseableA)
      intercept[Exception] {
        multipleAutoClosing(closeables) { _ =>
          throw new Exception
        }
      } .getSuppressed shouldBe empty
      assert(closeables.forall(_.closed))

    "with second Exception in close" in:
      val closeables = List(new AutoCloseableA, new FailingClose, new AutoCloseableA)
      val x = intercept[AException]:
        multipleAutoClosing(closeables) { _ =>
          throw new AException
        }
      assert(closeables collect { case a: A => a } forall { _.closed })
      for suppressed <- x.getSuppressed do
        suppressed.asInstanceOf[ClosedException]
  }

  "closeOnError" in:
    val closer = new Closer
    val ctx = new CloserTest.Context
    val a = new ctx.TestCloseable
    closer.register(a)
    closeOnError(closer) {}
    assert(!a.isClosed)
    intercept[IllegalStateException] { closeOnError(closer) { throw new IllegalStateException } }
    assert(a.isClosed)


private object AutoClosingTest:
  private class A extends AutoCloseable:
    var closed = false

    def close(): Unit =
      require(!closed)
      closed = true

  private final class FailingClose extends AutoCloseable:
    def close() = throw new ClosedException

  private final class AException extends Exception
  private final class ClosedException extends Exception("CLOSE ERROR")
