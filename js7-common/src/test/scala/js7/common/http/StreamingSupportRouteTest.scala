package js7.common.http

import cats.effect.Resource.ExitCase
import cats.effect.unsafe.IORuntime
import cats.effect.{Deferred, IO, ResourceIO}
import fs2.Stream
import js7.base.auth.SessionToken
import js7.base.catsutils.CatsDeadline
import js7.base.catsutils.CatsEffectExtensions.startAndForget
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.data.ByteArray
import js7.base.data.ByteSequence.ops.toAllByteSequenceOps
import js7.base.fs2utils.StreamExtensions.{interruptWhenF, onStart}
import js7.base.log.Logger
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.base.web.Uri
import js7.common.http.JsonStreamingSupport.`application/x-ndjson`
import js7.common.http.StreamingSupportRouteTest.*
import js7.common.pekkohttp.PekkoHttpServerUtils.completeWithByteStream
import js7.common.pekkohttp.WebLogDirectives
import js7.common.pekkohttp.web.PekkoWebServer
import js7.common.pekkohttp.web.PekkoWebServer.BoundRoute
import js7.common.pekkohttp.web.data.WebServerBinding
import js7.common.pekkoutils.Pekkos
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import org.apache.pekko.actor.ActorSystem
import org.scalatest.Assertion
import scala.concurrent.TimeoutException

final class StreamingSupportRouteTest extends OurAsyncTestSuite:

  private given IORuntime = ioRuntime

  private val config = config"""
    js7.web.server.shutdown-timeout = 10s
    js7.web.server.shutdown-delay = 500ms
    js7.web.server.auth.https-client-authentication = off
    pekko.http.server.idle-timeout = 2s  # In case, a stream is not cancelable
    """.withFallback(WebLogDirectives.TestConfig)

  "Stopping a simple quiet stream interruptWhen" in:
    for
      list <- (Stream.emit(1) ++ Stream.never[IO])
        .interruptWhen:
          Stream.sleep[IO](100.ms).as(true)
        .compile.toList
    yield
      assert(list == List(1))

  "Stop reading a HTTP stream" - {
    val interruptAfter = 500.ms
    val tooLong = 1.s
    val heartbeatChunk = ByteArray("HEARTBEAT\n")

    "Canceling a *quiet* stream with interruptWhen DOES NOT WORK" in:
      testStream().attempt.map:
        case Left(_: TimeoutException) =>
          info("As expected, a quiet Pekko HTTP stream is not cancelable with interruptWhen")
          succeed
        case Left(t) => throw t
        case Right(_) => fail("TEST UNEXPECTEDLY SUCCEEDED 🎉")

    "Canceling a *heartbeating* stream with interruptWhen" in:
      testStream(heartbeating = true)


    def testStream(heartbeating: Boolean = false): IO[Assertion] =
      val data = ByteArray("123\n")
      val port = findFreeTcpPort()
      val uri = Uri(s"http://localhost:$port")
      val serverStreamCanceled = Deferred.unsafe[IO, ExitCase]

      def webServerResource(using as: ActorSystem) =
        PekkoWebServer.resource(Seq(WebServerBinding.localhostHttp(port)), config): _ =>
          object route extends WebLogDirectives:
            val actorSystem = as
            val ioRuntime = StreamingSupportRouteTest.this.ioRuntime
            val config = StreamingSupportRouteTest.this.config
            val whenShuttingDown = Deferred.unsafe

            def boundRoute =
              BoundRoute.simple:
                webLog:
                  completeWithByteStream(`application/x-ndjson`):
                    Stream
                      .iterable(data.toArray)
                      .append(Stream.never[IO])
                      .through:
                        if heartbeating then
                          _.chunks
                            .keepAlive(interruptAfter / 10, IO.pure(heartbeatChunk.toChunk))
                            .unchunks
                        else
                          identity
                      .onFinalizeCase: exitCase =>
                        IO(logger.info(s"onFinalizeCase $exitCase")) *>
                          serverStreamCanceled.complete(exitCase).void
          route.boundRoute

      val clientResource: ResourceIO[PekkoHttpClient] =
        for
          given ActorSystem <- Pekkos.actorSystemResource("StreamingSupportRouteTest", config)
          _ <- webServerResource
          client <- PekkoHttpClient.resource(uri, uriPrefixPath = "")
        yield
          client

      clientResource.use: client =>
        given IO[Option[SessionToken]] = IO.none
        for
          since <- CatsDeadline.now
          stream <- client.getRawLinesStream(uri)
          deferred <- Deferred[IO, Unit]
          list <- stream
            .evalTap(o => IO(logger.info(s"<-<- $o")))
            .filter(_ != heartbeatChunk)
            .onStart:
              (IO.sleep(interruptAfter) *> deferred.complete(())).startAndForget
            .interruptWhenF:
              deferred.get *> IO(logger.info("🔺interruptWhen is being triggered"))
            .compile.toList
          // Server stream should being canceled now
          exitCase <- serverStreamCanceled.get
            .timeoutTo(1.s, IO.raiseError(new NotCanceledException))
          elapsed <- since.elapsed
          _ <- IO.raiseUnless(elapsed < tooLong)(new TimeoutException)
        yield
          assert(list == List(data) && exitCase == ExitCase.Canceled)
  }


object StreamingSupportRouteTest:
  private val logger = Logger[this.type]

  private final class NotCanceledException extends Exception
