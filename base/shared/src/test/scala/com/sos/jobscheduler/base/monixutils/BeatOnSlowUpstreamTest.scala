package com.sos.jobscheduler.base.monixutils

import com.sos.jobscheduler.base.monixutils.MonixBase.syntax._
import com.sos.jobscheduler.base.time.ScalaTime._
import monix.eval.Task
import monix.execution.schedulers.TestScheduler
import monix.reactive.Observable
import org.scalatest.FreeSpec
import scala.collection.immutable._
import scala.concurrent.Await

/**
  * @author Joacim Zschimmer
  */
final class BeatOnSlowUpstreamTest extends FreeSpec
{
  private implicit val scheduler = TestScheduler()
  private val heartbeat = -1

  "beatOnSlowUpstream" in {
    assert(runObservable(List(1, 2, 100, 200, 100)) ==
      List(1, 2, heartbeat, 100, heartbeat, heartbeat, 200, heartbeat, 100))
  }

  "beatOnSlowUpstream from start" in {
    assert(runObservable(List(200, 1, 2, 100)) ==
      List(heartbeat, heartbeat, 200, 1, 2, heartbeat, 100))
  }

  private def runObservable(milliseconds: Seq[Int]): Seq[Int] = {
    val future = Observable.fromIterable(milliseconds)
      .mapEval(i => Task(i).delayExecution(i.ms))
      .beatOnSlowUpstream(80.ms, heartbeat)
      .toListL
      .runToFuture
    for ( _ <- 1 to 100) scheduler.tick(80.ms)
    Await.result(future, 0.s)
  }
}
