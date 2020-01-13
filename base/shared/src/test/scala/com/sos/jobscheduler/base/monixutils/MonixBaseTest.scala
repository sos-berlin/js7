package com.sos.jobscheduler.base.monixutils

import com.sos.jobscheduler.base.monixutils.MonixBase._
import com.sos.jobscheduler.base.time.ScalaTime._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.AsyncFreeSpec
import scala.concurrent.TimeoutException
import scala.concurrent.duration.Duration

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
