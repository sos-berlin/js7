package js7.common.http

import cats.effect.IO
import cats.syntax.applicativeError.*
import fs2.Stream
import js7.base.catsutils.CatsEffectExtensions.fromOutcome
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.common.http.StreamingSupport.*
import js7.common.pekkoutils.Pekkos
import js7.common.pekkoutils.Pekkos.actorSystemResource
import org.apache.pekko.NotUsed
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.scaladsl.Source
import org.apache.pekko.stream.{DelayOverflowStrategy, FlowShape, Graph, KillSwitches, SharedKillSwitch}
import scala.concurrent.{Await, Future, TimeoutException}

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
      actorSystemResource("StreamingSupportTest", config = config).use: actorSystem =>
        given ActorSystem = actorSystem
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

    "FS2's timeoutOnPull with Future.never" in:
      pending // The test never ends.
      // TODO How to cancel a Pekko stream sticking in a source? Somehow with KillSwitch ...
      // cancel funktioniert, wenn eine Nachricht kommt (deshalb die Herzschläge überall)
      // Wenn keine Nachricht kommt (wegen Störung oder weniger Herzschläge genügen würden),
      // dann hängt cancel.
      // KillSwitch funktioniert, aber wie verbindet man das mit cancel?
      actorSystemResource("StreamingSupportTest").use: actorSystem =>
        given ActorSystem = actorSystem
        Source.future[String]:
          Future.never // Blocks, because not cancelable !!!
        .asFs2Stream()
        .timeoutOnPullTo(2.s, Stream.emit("TIMEOUT"))
        .compile
        .toList
        .map: list =>
          assert(list == List("TIMEOUT"))

  }

  "Cancel a Pekko stream with KillSwitch" - {
    "Cancel with Pekko's KillSwitch.single " in:
      import org.apache.pekko.stream.scaladsl.{Keep, Sink}
      actorSystemResource("StreamingSupportTest").use: actorSystem =>
        given ActorSystem = actorSystem

        IO:
          val n = 10000
          val countingSource: Source[Int, NotUsed] = Source(1 to n).delay(10.ms, DelayOverflowStrategy.backpressure)
          val lastSink = Sink.last[Int]
          val (killSwitch, last) =
            countingSource
              .viaMat(KillSwitches.single)(Keep.right)
              .toMat(lastSink)(Keep.both)
              .run()

          sleep(300.ms)
          killSwitch.shutdown()
          val result = Await.result(last, 10.s)
          assert(result > 10 && result < n)

    "Cancel with Pekko's KillSwitch.shared " in:
      final class PekkoStreamCanceller(name: String):
        private val killSwitch = KillSwitches.shared(name)

        def flow[A]: Graph[FlowShape[A, A], SharedKillSwitch] =
          killSwitch.flow[A]

        def cancel(throwable: Throwable = new TimeoutException): Unit =
          killSwitch.abort(throwable)

      actorSystemResource("StreamingSupportTest").use: actorSystem =>
        given ActorSystem = actorSystem
        val canceller = PekkoStreamCanceller("MY-KILL-SWITCH")

        Source.future[String]:
          Future.never
        .via(canceller.flow)
        .asFs2Stream()
        .recover:
          case _: TimeoutException => "CANCELED"
        .compile
        .toList
        .map: list =>
          assert(list == List("CANCELED"))
        .background.use: io =>
          IO.sleep(100.ms) *>
            IO(canceller.cancel()) *>
            io.flatMap(IO.fromOutcome)

    "Pekko's idleTimeout with Future.never" in :
      actorSystemResource("StreamingSupportTest").use: actorSystem =>
        given ActorSystem = actorSystem
        Source.future[String]:
          Future.never
        .idleTimeout(1.s)
        .asFs2Stream()
        .recover:
          case _: TimeoutException => "TIMEOUT"
        .compile
        .toList
        .map: list =>
          assert(list == List("TIMEOUT"))
  }
