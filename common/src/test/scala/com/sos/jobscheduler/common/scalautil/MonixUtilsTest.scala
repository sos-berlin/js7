package com.sos.jobscheduler.common.scalautil

import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.CloseableIterator
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.closeableIteratorToObservable
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.execution.{Cancelable, Scheduler}
import org.scalatest.FreeSpec
import scala.concurrent.Promise
import scala.concurrent.duration._
import scala.language.reflectiveCalls

/**
  * @author Joacim Zschimmer
  */
final class MonixUtilsTest extends FreeSpec
{
  "Scheduler.scheduleFor" in {
    val scheduler = Scheduler.global
    var called = false

    var cancelable = scheduler.scheduleFor(Timestamp("2500-01-01T00:00:00Z")) { called = true }
    assert(cancelable.isInstanceOf[Cancelable.IsDummy])
    sleep(10.ms)
    cancelable.cancel()
    assert(!called)

    var p = Promise[Unit]()
    scheduler.scheduleFor(Timestamp("1500-01-01T00:00:00Z")) { p.success(()) }
    p.future await  99.s

    p = Promise[Unit]()
    cancelable = scheduler.scheduleFor(Timestamp.now + 10.millis) { p.success(()) }
    p.future await 99.s
  }

  "closeableIteratorToObservable" in {
    val iterator = new CloseableIterator[Int] {
      var closed = false
      private val it = List(1, 2, 3).iterator
      def close() = closed = true
      def hasNext = it.hasNext
      def next() = it.next()
    }
    assert(!iterator.closed)
    val result = closeableIteratorToObservable(iterator).toListL await 99.s
    assert(result == List(1, 2, 3))
    assert(iterator.closed)
  }

  "materializeIntoChecked" in {
    assert(Task.pure(Checked(1)).materializeIntoChecked.runSyncUnsafe() == Checked(1))
    assert(Task.pure(Left(Problem("PROBLEM"))).materializeIntoChecked.runSyncUnsafe() == Left(Problem("PROBLEM")))
    assert(Task(sys.error("ERROR")).materializeIntoChecked.runSyncUnsafe() == Checked.catchNonFatal(sys.error("ERROR")))
  }
}
