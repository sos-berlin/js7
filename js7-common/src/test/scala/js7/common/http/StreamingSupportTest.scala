package js7.common.http

import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.common.pekkoutils.Pekkos
import js7.common.pekkoutils.Pekkos.newActorSystem
import js7.common.http.StreamingSupport.*
import cats.effect.IO
import monix.execution.Scheduler.Implicits.traced
import fs2.Stream
import scala.concurrent.Await

/**
  * @author Joacim Zschimmer
  */
final class StreamingSupportTest extends OurTestSuite:
  "Stream toPekkoSource" in:
    implicit val actorSystem = newActorSystem("StreamingSupportTest")

    var closed = 0
    val stream = Stream(1, 2, 3).guarantee(IO { closed += 1 })
    assert(Await.result(stream.toPekkoSource.runFold(0)(_ + _), 9.s) == 6)
    assert(closed == 1)

    Pekkos.terminateAndWait(actorSystem, 99.s)
