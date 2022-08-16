package js7.base.utils

import java.util.concurrent.ConcurrentLinkedQueue
import js7.base.test.OurTestSuite
import js7.base.utils.CloserTest.*
import monix.execution.atomic.AtomicBoolean
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.control.NoStackTrace

/**
  * @author Joacim Zschimmer
  */
final class CloserTest extends OurTestSuite
{
  "TestCloseable" in {
    val ctx = new Context
    val a = new ctx.TestCloseable
    a.close()
    val e = intercept[RuntimeException] { a.close() }
    assert(e.getMessage == "Duplicate close")
  }

  "LIFO" in {
    val closer = new Closer
    val ctx = new Context
    val a, b, c = new ctx.TestCloseable
    closer.register(a)
    closer.register(b)
    closer.register(c)
    closer.close()
    assert(ctx.closed == c :: b :: a :: Nil)
  }

  "Multiple exceptions" in {
    val closer = new Closer
    val ctx = new Context
    closer.register(new ctx.TestCloseable(Some(new TestException("A"))))
    closer.register(new ctx.TestCloseable(Some(new TestException("B"))))
    closer.register(new ctx.TestCloseable(Some(new TestException("C"))))
    val e = intercept[RuntimeException] {
      closer.close()
    }
    assert(e.getMessage == "C")
    assert(e.getSuppressed()(0).getMessage == "B")
    assert(e.getSuppressed()(1).getMessage == "A")
  }

  "Fatal exception has priority" in {
    val closer = new Closer
    val ctx = new Context
    closer.register(new ctx.TestCloseable(Some(new TestException("A"))))
    closer.register(new ctx.TestCloseable(Some(new OutOfMemoryError)))
    closer.register(new ctx.TestCloseable(Some(new TestException("C"))))
    val e = intercept[OutOfMemoryError] {
      closer.close()
    }
    assert(e.getSuppressed.isEmpty)
  }

  "onClose" in {
    val closer = new Closer
    var a = false
    closer.onClose { a = true }
    assert(!a)
    closer.close()
    assert(a)
  }

  "onCloseOrShutdown" in {
    var closed = false
    val closer = new Closer
    closer.onCloseOrShutdown { closed = true }
    assert(!closed)
    closer.close()
    assert(closed)
  }

  "closeThen" in {
    val closes = mutable.Buffer.empty[Int]
    val closer = new Closer
    closer.onClose { closes += 1 }
    closer.closeThen { closes += 2 }
    assert(closes == List(1, 2))
  }

  "syntax" - {
    import Closer.syntax.*

    "closeWithCloser AutoClosable" in {
      implicit val closer = new Closer
      val ctx = new Context
      val closeable = new ctx.TestCloseable
      closeable.closeWithCloser
      closer.close()
      assert(closeable.isClosed)
      closer.close()
    }

    "AnyRef.withCloser" in {
      implicit val closer = new Closer
      class A
      var closedA: A = null
      val a = new A
      val c = a.withCloser { a => closedA = a }
      assert(a eq c)
      assert(closedA == null)
      closer.close()
      assert(closedA == c)
    }
  }

  "object methods" - {
    import Closer.*

    "withCloser" in {
      val ctx = new Context
      val closeable = new ctx.TestCloseable
      assert(!closeable.isClosed)
      withCloser { closer =>
        closer.register(closeable)
      }
      assert(closeable.isClosed)
    }

    "closeOrdered" in {
      val closed = mutable.Buffer.empty[Any]
      val a = new AutoCloseable { def close() = closed += this }
      val b = new AutoCloseable { def close() = closed += this }
      closeOrdered(a, b)
      assert(closed == List(a, b))
    }
  }
}

object CloserTest
{
  private[utils] final class Context {
    private val closeables = new ConcurrentLinkedQueue[TestCloseable]

    def closed = closeables.asScala.toList

    final class TestCloseable(throwable: Option[Throwable] = None) extends AutoCloseable {
      private val closed = AtomicBoolean(false)

      def close() = {
        if (closed.getAndSet(true)) sys.error("Duplicate close")
        closeables.add(this)
        for (t <- throwable) throw t
      }

      def isClosed = closed.get()
    }
  }

  private final class TestException(message: String) extends RuntimeException(message) with NoStackTrace
}
