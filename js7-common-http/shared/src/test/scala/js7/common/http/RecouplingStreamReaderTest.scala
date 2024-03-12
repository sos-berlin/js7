package js7.common.http

import cats.effect.IO
import cats.effect.testkit.TestControl
import fs2.Stream
import js7.base.auth.{UserAndPassword, UserId}
import js7.base.generic.SecretString
import js7.base.problem.Problem
import js7.base.session.TestSessionApi
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.Nel
import js7.common.http.configuration.RecouplingStreamReaderConf

/**
  * @author Joacim Zschimmer
  */
final class RecouplingStreamReaderTest extends OurAsyncTestSuite:

  "RecouplingStreamReader" in:
    val userAndPassword = UserAndPassword(UserId("USER"), SecretString("PASSWORD"))
    val api = new TestSessionApi(Some(userAndPassword))
    val recouplingStreamReaderConf = RecouplingStreamReaderConf(
      timeout = 5.s, keepAlive = 1.s, delay = 1.s,
      failureDelays = Nel.one(5.s))

    val stream = Stream.suspend:
      @volatile var lastErrorAt = -2

      def getUnderlyingStream(after: Long) =
        IO:
          lastErrorAt match
            case -2 =>
              lastErrorAt = -1
              throw new IllegalArgumentException("GET-ERROR")
            case -1 =>
              lastErrorAt = 0
              Left(Problem("TEST-PROBLEM"))
            case _ =>
              Right:
                Stream.fromIterator[IO](
                  iterator = Iterator.from(after.toInt + 1)
                    .map:
                      case 3 if lastErrorAt != 3 =>
                        lastErrorAt = 3
                        throw new IllegalArgumentException("TEST-ERROR")
                      case i => i
                    .map(_.toString),
                  chunkSize = 1)

      RecouplingStreamReader.stream[Long, String, TestSessionApi](
        toIndex = _.toLong,
        api,
        recouplingStreamReaderConf,
        after = 0L,
        getStream = getUnderlyingStream)

    TestControl.executeEmbed:
      for
        list <- stream.take(10).compile.toList.timeout(99.s)
        _ = assert(list == (1 to 10).map(_.toString).toList)

        // Test cancellation
        streaming <- stream.evalTap(_ => IO.sleep(10.ms))
          .compile.toList
          .onCancel(IO.raiseError(new RuntimeException("TEST")))
          .start
        _ <- IO.sleep(50.ms)
        _ <- streaming.cancel
        outcome <- streaming.join
      yield
        assert(outcome.isCanceled)
