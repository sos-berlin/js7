package com.sos.jobscheduler.common.scalautil

import com.sos.jobscheduler.common.scalautil.CloserTest._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import java.util.concurrent.ConcurrentLinkedQueue
import monix.execution.atomic.AtomicBoolean
import org.scalatest.FreeSpec
import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._

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
    closer.register(new ctx.TestCloseable(Some(new RuntimeException("A"))))
    closer.register(new ctx.TestCloseable(Some(new RuntimeException("B"))))
    closer.register(new ctx.TestCloseable(Some(new RuntimeException("C"))))
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
    closer.register(new ctx.TestCloseable(Some(new RuntimeException("A"))))
    closer.register(new ctx.TestCloseable(Some(new OutOfMemoryError)))
    closer.register(new ctx.TestCloseable(Some(new RuntimeException("C"))))
    val e = intercept[OutOfMemoryError] {
      closer.close()
    }
    assert(e.getSuppressed.isEmpty)
  }

  "Threads" in {
    val closer = new Closer
    val ctx = new Context
    val closeables = Vector.fill(100000) { new ctx.TestCloseable }
    for (c ← closeables) closer.register(c)
    (1 to sys.runtime.availableProcessors).map(_ ⇒ Future(closer.close())) await 99.seconds
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
}

object CloserTest {
  private class Context {
    private val closeables = new ConcurrentLinkedQueue[TestCloseable]

    def closed = closeables.asScala.toList

    final class TestCloseable(throwable: Option[Throwable] = None) extends AutoCloseable {
      private val closed = AtomicBoolean(false)

      def close() = {
        if (!closed.compareAndSet(false, true)) sys.error("Duplicate close")
        closeables.add(this)
        for (t ← throwable) throw t
      }

      def isClosed = closed.get
    }
  }
}
