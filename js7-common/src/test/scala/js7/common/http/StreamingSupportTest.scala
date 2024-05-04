package js7.common.http

import cats.effect.IO
import fs2.Stream
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.common.http.StreamingSupport.*
import js7.common.pekkoutils.Pekkos
import js7.common.pekkoutils.Pekkos.actorSystemResource
import org.apache.pekko.stream.scaladsl.Source
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
final class StreamingSupportTest extends OurAsyncTestSuite:

  "toPekkoSourceForHttpResponse" in:
    actorSystemResource("StreamingSupportTest")
      .use(implicit actorSystem =>
        var closed = 0
        val stream = Stream(1, 2, 3).covary[IO].onFinalize(IO:
          closed += 1)
        for
          result <- stream
            .toPekkoSourceForHttpResponse
            .use: source =>
              IO.fromFuture(IO:
                source.runFold(0)(_ + _))
        yield assert:
          result == 6 && closed == 1)

  "asFs2Stream" - {
    "Terminate a Stream originating from a Future" in:
      val config = config"pekko.scheduler.tick-duration = 1.ms"
      actorSystemResource("StreamingSupportTest", config = config)
        .use { implicit actorSystem =>
          var last = -1
          for
            stream <- IO.fromFuture(IO(Future:
              Source
                .tick(0.s, 1.ms, ())
                .zip(Source(1 to Int.MaxValue))
                .map(_._2)
                .map: i =>
                  last = i
                  i
                .asFs2Stream()))
            list <- stream.take(3).compile.toList
          yield
            assert(list == List(1, 2, 3) && last == 3)
        }
  }