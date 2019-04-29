package com.sos.jobscheduler.common.scalautil

import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.MemoizerTest._
import com.sos.jobscheduler.common.time.ScalaTime._
import java.util.concurrent.atomic.AtomicInteger
import org.scalatest.FreeSpec
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
final class MemoizerTest extends FreeSpec {

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
    val calls = testConcurrency { f => Memoizer.nonStrict(f) }
    assert(calls >= Arguments.size && calls < ParallelCount * Arguments.size)
  }

  "strict" in {
    val calls = testConcurrency { f => Memoizer.strict(f) }
    assert(calls == Arguments.size)
  }

  private def testConcurrency(memoizer: (Int => String) => Int => String): Int = {
    val calls = new AtomicInteger
    def f(a: Int) = {
      calls.incrementAndGet()
      sleep(10.ms)
      s"/$a/"
    }
    val m = memoizer(f)
    val result = (for (_ <- 1 to ParallelCount) yield Future { for (a <- Arguments) yield m(a) }) await 99.s
    for (r <- result) assert(r == (Arguments map { o => s"/$o/" }))
    calls.get
  }
}

private object MemoizerTest {
  private val ParallelCount = 100 * sys.runtime.availableProcessors/*Thread count of ExecutionContext*/
  private val Arguments = 1 to 5
}
