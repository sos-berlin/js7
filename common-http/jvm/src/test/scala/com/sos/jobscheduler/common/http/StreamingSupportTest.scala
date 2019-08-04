package com.sos.jobscheduler.common.http

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.http.StreamingSupport._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.FreeSpec
import scala.concurrent.Await
import scala.language.reflectiveCalls

/**
  * @author Joacim Zschimmer
  */
final class StreamingSupportTest extends FreeSpec
{
  "Observable toAkkaSource" in {
    val actorSystem = ActorSystem("StreamingSupportTest")
    implicit val materializer = ActorMaterializer.create(actorSystem)

    var closed = 0
    val observable = Observable(1, 2, 3).guarantee(Task { closed += 1 })
    assert(Await.result(observable.toAkkaSource.runFold(0)(_ + _), 9.s) == 6)
    assert(closed == 1)

    materializer.shutdown()
    actorSystem.terminate()
  }
}
