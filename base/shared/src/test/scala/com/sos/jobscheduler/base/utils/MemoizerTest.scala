package com.sos.jobscheduler.base.utils

import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.MemoizerTest._
import java.util.concurrent.atomic.AtomicInteger
import org.scalatest.freespec.AsyncFreeSpec
import scala.collection.mutable
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
final class MemoizerTest extends AsyncFreeSpec
{
  "Unary" in {
    val called = mutable.Buffer[Int]()
    def f(a: Int) = {
      called += a
      s"/$a/"
    }
    val m: Int => String = Memoizer.nonStrict(f)
    assert(m(1) == "/1/")
    assert(called == List(1))
    assert(m(2) == "/2/")
    assert(called == List(1, 2))
    assert(m(1) == "/1/")
    assert(called == List(1, 2))
    assert(m(2) == "/2/")
    assert(called == List(1, 2))
  }

  "Binary" in {
    val called = mutable.Buffer[(Int, Boolean)]()
    def f(a: Int, b: Boolean) = {
      called += ((a, b))
      s"$a $b"
    }
    val m: (Int, Boolean) => String = Memoizer.nonStrict(f)
    assert(m(1, false) == "1 false")
    assert(called == List((1, false)))
    assert(m(1, true) == "1 true")
    assert(called == List((1, false), (1, true)))
    assert(m(2, true) == "2 true")
    assert(called == List((1, false), (1, true), (2, true)))
    assert(m(2, true) == "2 true")
    assert(called == List((1, false), (1, true), (2, true)))
  }

  "Concurrency" in {
    testConcurrency { f => Memoizer.nonStrict(f) }
      .map(calls =>
        assert(calls >= Arguments.size && calls < ParallelCount * Arguments.size))
  }

  "strict" in {
    testConcurrency { f => Memoizer.strict(f) }
      .map(calls => assert(calls ==  Arguments.size))
  }

  private def testConcurrency(memoizer: (Int => String) => Int => String): Future[Int] = {
    val calls = new AtomicInteger
    def f(a: Int) = {
      calls.incrementAndGet()
      sleep(10.ms)
      s"/$a/"
    }
    val m = memoizer(f)
    Future.sequence(for (_ <- 1 to ParallelCount) yield Future { for (a <- Arguments) yield m(a) })
      .map { result =>
        for (r <- result) assert(r == (Arguments map { o => s"/$o/" }))
        calls.get
      }
  }
}

private object MemoizerTest {
  private val ParallelCount = 100 * sys.runtime.availableProcessors/*Thread count of ExecutionContext*/
  private val Arguments = 1 to 5
}
