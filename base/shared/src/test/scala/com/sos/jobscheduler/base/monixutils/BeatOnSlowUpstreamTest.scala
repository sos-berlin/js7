package com.sos.jobscheduler.base.monixutils

import com.sos.jobscheduler.base.monixutils.MonixBase.RichMonixObservable
import com.sos.jobscheduler.base.time.ScalaTime._
import monix.eval.Task
import monix.execution.schedulers.TestScheduler
import monix.reactive.Observable
import org.scalatest.FreeSpec
import scala.concurrent.Await

/**
  * @author Joacim Zschimmer
  */
final class BeatOnSlowUpstreamTest extends FreeSpec
{
  private implicit val scheduler = TestScheduler()

  "beatOnSlowUpstream" in {
    val heartbeat = -1
    val future = Observable(1, 2, 100, 200, 100)
      .mapEval(i => Task(i).delayExecution(i.ms))
      .beatOnSlowUpstream(80.ms, heartbeat)
      .toListL
      .runToFuture
    for ( _ <- 1 to 10) scheduler.tick(80.ms)
    val result = Await.result(future, 0.s)
    assert(result == 1 :: 2 :: heartbeat :: 100 :: heartbeat :: heartbeat :: 200 :: heartbeat :: 100 :: Nil)
  }
}
