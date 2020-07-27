package js7.base.monixutils

import js7.base.monixutils.MonixBase._
import js7.base.monixutils.MonixBase.syntax._
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.base.utils.CloseableIterator
import monix.eval.Task
import monix.execution.Cancelable
import monix.execution.Scheduler.Implicits.global
import monix.execution.atomic.AtomicInt
import monix.execution.schedulers.TestScheduler
import org.scalatest.freespec.AsyncFreeSpec
import scala.concurrent.TimeoutException
import scala.concurrent.duration._
import scala.language.reflectiveCalls

/**
  * @author Joacim Zschimmer
  */
final class MonixBaseTest extends AsyncFreeSpec
{
  private val sleepyTask = Task(3).delayExecution(200.ms)

  "maybeTimeout Duration.Inf" in {
    sleepyTask.maybeTimeout(Duration.Inf)
      .map(o => assert(o == 3))
      .runToFuture
  }

  "maybeTimeout FiniteDuration" in {
    sleepyTask.map(_ => assert(false))
      .maybeTimeout(10.ms)
      .onErrorHandle(t => assert(t.isInstanceOf[TimeoutException]))
      .runToFuture
  }

  "orTimeout" in {
    sleepyTask
      .orTimeout(10.ms, Task(7))
      .map(o => assert(o == 7))
      .runToFuture
  }

  "materializeIntoChecked Right(value)" in {
    Task.pure(Checked(1))
      .materializeIntoChecked
      .runToFuture
      .map(o =>
        assert(o == Checked(1)))
  }

  "materializeIntoChecked Left(problem)" in {
    Task.pure(Left(Problem("PROBLEM")): Checked[Int])
      .materializeIntoChecked
      .runToFuture
      .map(o =>
        assert(o == Left(Problem("PROBLEM"))))
  }

  "materializeIntoChecked exception" in {
    Task(sys.error("ERROR"): Checked[Int])
      .materializeIntoChecked
      .runToFuture
      .map(o =>
        assert(o == Checked.catchNonFatal(sys.error("ERROR"))))
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
    closeableIteratorToObservable(iterator).toListL
      .runToFuture.map { result =>
        assert(result == List(1, 2, 3))
        assert(iterator.closed)
      }
  }

  "Scheduler convenience methods" - {
    "scheduleFor far future" in {
      val scheduler = TestScheduler()
      var called = false
      var cancelable = scheduler.scheduleFor(Timestamp("2500-01-01T00:00:00Z")) { called = true }
      assert(cancelable.isInstanceOf[Cancelable.IsDummy])
      scheduler.tick()
      cancelable.cancel()
      assert(!called)
    }

    "scheduleFor near future" in {
      val scheduler = TestScheduler()
      var called = false
      scheduler.scheduleFor(Timestamp.ofEpochMilli(scheduler.clockRealTime(MILLISECONDS)) + 1.s) { called = true }
      scheduler.tick()
      assert(!called)
      scheduler.tick(1.s)
      assert(called)
    }

    "scheduleFor past" in {
      val scheduler = TestScheduler()
      var called = false
      scheduler.scheduleFor(Timestamp("1500-01-01T00:00:00Z")) { called = true }
      scheduler.tick()
      assert(called)
    }

    "scheduleAtFixedRates" in {
      val scheduler = TestScheduler()
      val i = AtomicInt(0)
      val cancelable = scheduler.scheduleAtFixedRates(Array(2.s, 3.s, 4.s)) { i += 1 }
      for (expected <- Array(0, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6)) {
        scheduler.tick(1.s)
        assert(i.get() == expected)
      }
      cancelable.cancel()
      scheduler.tick(100.s)
      assert(i.get() == 6)
    }
  }

  "durationOfTask" in {
    durationOfTask(Task.pure(7).delayResult(10.ms))
      .map(o =>
        assert(o._1 == 7 && o._2 >= 10.ms))
      .runToFuture
  }

  //"takeUntil memory leak" in {
  //  val promise = Promise[Unit]()
  //  val stop = Observable.empty //Observable.fromFuture(promise.future)  // Memory leak - doesn't matter if called only once
  //  val obs = Observable.tailRecM(10000000) {
  //    case 0 => Observable.pure(Right(()))
  //    case i => Observable.pure(Left(i - 1)) takeUntilEval Task.never
  //  }
  //  obs.completedL
  //    .map(_ => assert(true))
  //    .runToFuture
  //}
}
