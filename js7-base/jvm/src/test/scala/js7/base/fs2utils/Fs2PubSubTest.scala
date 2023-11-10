package js7.base.fs2utils

import cats.effect.{FiberIO, IO, Outcome, OutcomeIO}
import cats.syntax.parallel.*
import js7.base.test.OurAsyncTestSuite
import js7.base.utils.Tests.isIntelliJIdea
import org.scalatest.{Assertion, Succeeded}
import fs2.Stream

final class Fs2PubSubTest extends OurAsyncTestSuite:

  "FsPubSub with several newStream" in:
    val n = 10000

    def startStream[A <: AnyRef](pubSub: Fs2PubSub[IO, A]): IO[FiberIO[Vector[A]]] =
      pubSub
        .newStream
        .flatMap(_.allocated)
        .flatMap((stream, release) =>
          stream.compile
            .toVector
            .guarantee(release)
            .start)

    def test(nr: Int): IO[Assertion] =
      Fs2PubSub.resource[IO, String].use(pubSub =>
        for
          _ <- pubSub.publish("IGNORED")
          streaming1 <- startStream(pubSub)
          _ <- pubSub.publish("EINS")
          streaming2 <- startStream(pubSub)
          _ <- pubSub.publish("ZWEI")
          streaming3 <- startStream(pubSub)
          _ <- pubSub.close
          streaming4 <- startStream(pubSub)
          result1 <- asSucceeded(streaming1.join)
          result2 <- asSucceeded(streaming2.join)
          result3 <- asSucceeded(streaming3.join)
          result4 <- asSucceeded(streaming4.join)
        yield withClue(s"#$nr: "):
          assert(result1 == Vector("EINS", "ZWEI"))
          assert(result2 == Vector("ZWEI"))
          assert(result3 == Vector())
          assert(result4 == Vector())
          if isIntelliJIdea && nr % 100 == 0 then print("✔")
          succeed)

    (1 to n).toVector.parTraverse(i => test(i))
      .flatTap(results => IO(assert(results.size == n && results.forall(_ == Succeeded))))
      .as(succeed)

  private def asSucceeded[A](outcomeIO: IO[OutcomeIO[A]]): IO[A] =
    for
      outcome <- outcomeIO
      a <-
        val Outcome.Succeeded(io) = outcome: @unchecked
        io
    yield a
