package js7.common.http

import cats.effect.IO
import fs2.Stream
import js7.base.test.{OurAsyncTestSuite, TestCatsEffect}
import js7.base.time.ScalaTime.*
import js7.common.http.StreamingSupport.*
import js7.common.pekkoutils.Pekkos
import js7.common.pekkoutils.Pekkos.actorSystemResource

/**
  * @author Joacim Zschimmer
  */
final class StreamingSupportTest extends OurAsyncTestSuite, TestCatsEffect:

  "Stream toPekkoSource" in:
    actorSystemResource("StreamingSupportTest")
      .use(implicit actorSystem =>
        var closed = 0
        val stream = Stream(1, 2, 3).covary[IO].onFinalize(IO:
          closed += 1)
        for
          result <- stream
            .toPekkoSourceResource
            .use: source =>
              IO.fromFuture(IO:
                source.runFold(0)(_ + _))
        yield assert:
          result == 6 && closed == 1)
