package js7.base.monixutils

import cats.effect.ExitCase
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
import monix.reactive.Observable
import org.scalatest.freespec.AsyncFreeSpec
import scala.concurrent.TimeoutException
import scala.concurrent.duration._
import scala.language.reflectiveCalls

/**
  * @author Joacim Zschimmer
  */
final class MonixBaseTest extends AsyncFreeSpec
{
  "maybeTimeout Duration.Inf" in {
    Task(3).delayExecution(200.ms)
      .maybeTimeout(Duration.Inf)
      .map(o => assert(o == 3))
      .runToFuture
  }

  "maybeTimeout FiniteDuration" in {
    Task(3).delayExecution(99.s)
      .maybeTimeout(0.s)
      .map(_ => assert(false))
      .onErrorHandle(t => assert(t.isInstanceOf[TimeoutException]))
      .runToFuture
  }

  "orTimeout" in {
    Task(3).delayExecution(99.s)
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

  "Observable" - {
    "to(List)" in {
      Observable(1, 2, 3).toL(List)
        .map((o: List[Int]) => assert(o  == List(1, 2, 3)))
        .runToFuture
    }

    "to(Vector)" in {
      Observable(1, 2, 3).toL(Vector)
        .map((o: Vector[Int]) => assert(o  == Vector(1, 2, 3)))
        .runToFuture
    }

    "to(Set)" in {
      Observable(1, 2, 2, 3).toL(Set)
        .map((o: Set[Int]) => assert(o  == Set(1, 2, 3)))
        .runToFuture
    }

    "tapEach catches exception" in {
      Observable.range(1, 100)
        .tapEach {
          case 2 => throw new IllegalArgumentException("TEST")
          case _ =>
        }
        .onErrorRecover { case e: RuntimeException if e.getMessage == "TEST" => -1 }
        .toListL
        .map(list => assert(list == List(1, -1)))
        .runToFuture
    }

    "mapParallelOrderedBatch" in {
      val n = 7777
      Observable.range(0, n)
        .mapParallelOrdered(sys.runtime.availableProcessors)(o => Task(o * -1))
        .toListL
        .map(list => assert(list == (0 until n).map(_ * -1)))
        .runToFuture
    }

    "mapParallelUnorderedBatch" in {
      val n = 7777
      Observable.range(0, n)
        .mapParallelUnorderedBatch()(_ * -1)
        .toListL
        .map(list => assert(list.toSet == (0 until n).map(_ * -1).toSet))
        .runToFuture
    }

    "updateState" in {
      Observable(1, 2, 3, 4, 5)
        .updateState(0) { case (state, int) => state + int }
        .takeWhileInclusive(_._1 != 10)
        .map(_._2)
        .toListL
        .map(list => assert(list == List(1, 2, 3, 4)))
        .runToFuture
    }

    "updateStateWhileInclusive" in {
      Observable(1, 2, 3, 4, 5)
        .updateStateWhileInclusive(0)(_ != 10) { case (state, int) => state + int }
        .toListL
        .map(list => assert(list == List(1, 2, 3, 4)))
        .runToFuture
    }

    "logTiming" in {
      val n = 7777
      var duration: FiniteDuration = null
      var count: Long = 0
      var exitCase: ExitCase[Throwable] = null
      Observable.range(0, n)
        .logTiming(_ => 2, (d, n, e) => {
          duration = d
          count = n
          exitCase = e
        })
        .completedL
        .map(_ => assert(duration > 0.s && count == 2 * n && exitCase == ExitCase.Completed))
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
}
