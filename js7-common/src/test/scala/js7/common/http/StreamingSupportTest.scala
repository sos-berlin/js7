package js7.common.http

import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.common.pekkoutils.Pekkos
import js7.common.pekkoutils.Pekkos.newActorSystem
import js7.common.http.StreamingSupport.*
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import monix.reactive.Observable
import scala.concurrent.Await

/**
  * @author Joacim Zschimmer
  */
final class StreamingSupportTest extends OurTestSuite:
  "Observable toPekkoSource" in:
    implicit val actorSystem = newActorSystem("StreamingSupportTest")

    var closed = 0
    val observable = Observable(1, 2, 3).guarantee(Task { closed += 1 })
    assert(Await.result(observable.toPekkoSource.runFold(0)(_ + _), 9.s) == 6)
    assert(closed == 1)

    Pekkos.terminateAndWait(actorSystem, 99.s)
