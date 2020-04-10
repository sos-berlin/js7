package com.sos.jobscheduler.master.cluster

import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.master.cluster.ObservablePauseDetector._
import monix.eval.Task
import monix.execution.schedulers.TestScheduler
import monix.reactive.Observable
import scala.util.Success
import org.scalatest.freespec.AnyFreeSpec

final class ObservablePauseDetectorTest extends AnyFreeSpec
{
  private implicit val scheduler = TestScheduler()
  private val pausingObservable = Observable.fromIterable(1 to 5)
    .doOnNext(i =>
      Task.sleep(if (i == 3) 2.s else 100.ms)) // Long pause before the third element

  "detectPauses" in {
    val future = pausingObservable.detectPauses(1.s).takeWhileInclusive(_ != Some(5)).toListL.runToFuture
    scheduler.tick(6.s)
    //for (_ <- 1 to 11) scheduler.tick(500.ms)
    assert(future.value == Some(Success(Some(1) :: Some(2) :: None :: Some(3) :: Some(4) :: Some(5) :: Nil)))
  }

  "detectPauses detects initial pause" in {
    val future = (Observable.fromTask(Task(0).delayExecution(2.s)) ++ pausingObservable)
      .detectPauses(1.s).takeWhileInclusive(_ != Some(5)).toListL.runToFuture
    scheduler.tick(6.s)
    //for (_ <- 1 to 11) scheduler.tick(500.ms)
    assert(future.value == Some(Success(None :: Some(0) :: Some(1) :: Some(2) :: None :: Some(3) :: Some(4) :: Some(5) :: Nil)))
  }

  "echoRepeated"  in {
    val future = pausingObservable.echoRepeated(800.ms).toListL.runToFuture
    scheduler.tick(6.s)
    //for (_ <- 1 to 11) scheduler.tick(500.ms)
    assert(future.value == Some(Success(List(1, 2, 2/*echo*/, 2/*echo*/, 3, 4, 5))))
  }
}
