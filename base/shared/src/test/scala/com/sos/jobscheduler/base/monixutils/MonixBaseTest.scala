package com.sos.jobscheduler.base.monixutils

import com.sos.jobscheduler.base.monixutils.MonixBase._
import com.sos.jobscheduler.base.monixutils.MonixBase.syntax._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.CloseableIterator
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.TimeoutException
import scala.concurrent.duration.Duration
import scala.language.reflectiveCalls
import org.scalatest.freespec.AsyncFreeSpec

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
