package js7.base.fs2utils

import cats.effect.concurrent.Deferred
import cats.effect.{ContextShift, Fiber, IO}
import cats.syntax.flatMap.*
import cats.syntax.parallel.*
import fs2.Stream
import js7.base.test.OurAsyncTestSuite
import js7.base.utils.Tests.isIntelliJIdea
import monix.execution.Scheduler
import org.scalatest.{Assertion, Succeeded}

final class Fs2PubSubTest extends OurAsyncTestSuite
{
  private implicit val contextShift: ContextShift[IO] =
    IO.contextShift(Scheduler.traced)

  "FsPubSub" in {
    val n = 10000

    def startStream[A <: AnyRef](pubSub: Fs2PubSub[IO, A], require: Int): IO[Fiber[IO, Vector[A]]] = {
      val initial = null.asInstanceOf[A]
      for
        streamStarted <- Deferred[IO, Unit]
        streamFiber <- pubSub
          .newStream
          .map(Stream.emit(initial).append(_).zipWithIndex)
          .flatMap(_
            .evalTap {
              case (_, i) if i == require => streamStarted.complete(()).attempt
              case _ => IO.unit
            }
            .filter(_._2 > require)
            .map(_._1)
            .compile
            .toVector)
          .start
        _ <- streamStarted.get
      yield streamFiber
    }

    def test(nr: Int): IO[Assertion] = {
      val pubSub = new Fs2PubSub[IO, String]
      for
        _ <- pubSub.publish("NULL")
        streaming1 <- startStream(pubSub, 1/*read last push "NULL" before continuing*/)
        _ <- pubSub.publish("EINS")
        streaming2 <- startStream(pubSub, 1/*read last push "EINS" before continuing*/)
        _ <- pubSub.publish("ZWEI")
        streaming3 <- startStream(pubSub, 1/*read last push "ZWEI" before continuing*/)
        _ <- pubSub.complete
        streaming4 <- startStream(pubSub, 0)
        result1 <- streaming1.join
        result2 <- streaming2.join
        result3 <- streaming3.join
        result4 <- streaming4.join
      yield withClue(s"#$nr: ") {
        assert(result1 == Vector("EINS", "ZWEI"))
        assert(result2 == Vector("ZWEI"))
        assert(result3 == Vector())
        assert(result4 == Vector())
        if isIntelliJIdea && nr % 100 == 0 then print(s"✔")
        succeed
      }
    }

    (1 to n).toVector.parTraverse(i => test(i))
      .flatTap(results => IO(assert(results.size == n && results.forall(_ == Succeeded))))
      .as(succeed)
      .unsafeToFuture()
  }
}
