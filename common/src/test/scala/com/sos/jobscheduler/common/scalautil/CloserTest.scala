package com.sos.jobscheduler.common.scalautil

import com.sos.jobscheduler.common.scalautil.CloserTest._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import java.util.concurrent.ConcurrentLinkedQueue
import monix.execution.atomic.AtomicBoolean
import org.mockito.Mockito.{never, verify}
import org.scalatest.FreeSpec
import org.scalatestplus.mockito.MockitoSugar.mock
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.control.NoStackTrace

/**
  * @author Joacim Zschimmer
  */
final class CloserTest extends FreeSpec
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

  "Threads" in {
    val closer = new Closer
    val ctx = new Context
    val closeables = Vector.fill(100000) { new ctx.TestCloseable }
    for (c <- closeables) closer.register(c)
    (1 to sys.runtime.availableProcessors).map(_ => Future(closer.close())) await 99.seconds
    assert(closeables forall (_.isClosed))
    assert(ctx.closed.toSet == closeables.toSet)
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
    val closes = mutable.Buffer[Int]()
    val closer = new Closer
    closer.onClose { closes += 1 }
    closer.closeThen { closes += 2 }
    assert(closes == List(1, 2))
  }

  "ops" - {
    import Closer.ops._
    "closeWithCloser AutoClosable" in {
      implicit val closer = new Closer
      val c = mock[AutoCloseable].closeWithCloser
      verify(c, never).close()
      closer.close()
      verify(c).close()
    }

    "AnyRef.withCloser" in {
      implicit val closer = new Closer
      trait A
      var closedA: A = null
      val c = mock[A].withCloser { a => closedA = a }
      assert(closedA == null)
      closer.close()
      assert(closedA == c)
    }
  }

  "object methods" - {
    import Closer._

    "withCloser" in {
      val a = mock[AutoCloseable]
      withCloser { closer =>
        closer.register(a)
      }
      verify(a).close()
    }

    "closeOrdered" in {
      val closed = mutable.Buffer[Any]()
      val a = new AutoCloseable { def close() = closed += this }
      val b = new AutoCloseable { def close() = closed += this }
      closeOrdered(a, b)
      assert(closed == List(a, b))
    }
  }
}

object CloserTest {
  private class Context {
    private val closeables = new ConcurrentLinkedQueue[TestCloseable]

    def closed = closeables.asScala.toList

    final class TestCloseable(throwable: Option[Throwable] = None) extends AutoCloseable {
      private val closed = AtomicBoolean(false)

      def close() = {
        if (closed.getAndSet(true)) sys.error("Duplicate close")
        closeables.add(this)
        for (t <- throwable) throw t
      }

      def isClosed = closed.get
    }
  }

  private final class TestException(message: String) extends RuntimeException(message) with NoStackTrace
}
