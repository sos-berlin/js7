package js7.common.http

import js7.base.test.Test
import js7.base.time.ScalaTime.*
import js7.common.akkautils.Akkas
import js7.common.akkautils.Akkas.newActorSystem
import js7.common.http.StreamingSupport.*
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import monix.reactive.Observable
import scala.concurrent.Await

/**
  * @author Joacim Zschimmer
  */
final class StreamingSupportTest extends Test
{
  "Observable toAkkaSource" in {
    implicit val actorSystem = newActorSystem("StreamingSupportTest")

    var closed = 0
    val observable = Observable(1, 2, 3).guarantee(Task { closed += 1 })
    assert(Await.result(observable.toAkkaSource.runFold(0)(_ + _), 9.s) == 6)
    assert(closed == 1)

    Akkas.terminateAndWait(actorSystem, 99.s)
  }
}
