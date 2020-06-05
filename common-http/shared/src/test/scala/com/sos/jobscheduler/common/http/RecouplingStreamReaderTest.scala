package js7.common.http

import js7.base.auth.{UserAndPassword, UserId}
import js7.base.generic.SecretString
import js7.base.problem.Problem
import js7.base.session.TestSessionApi
import js7.base.time.ScalaTime._
import js7.common.http.configuration.RecouplingStreamReaderConf
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.freespec.AsyncFreeSpec
import scala.concurrent.Await

/**
  * @author Joacim Zschimmer
  */
final class RecouplingStreamReaderTest extends AsyncFreeSpec
{
  "RecouplingStreamReader" in {
    val userAndPassword = UserAndPassword(UserId("USER"), SecretString("PASSWORD"))
    val api = new TestSessionApi(Some(userAndPassword))
    val recouplingStreamReaderConf = RecouplingStreamReaderConf(timeout = 5.s, delay = 1.s)

    val observable = Observable.defer {
      @volatile var lastErrorAt = -2
      def getUnderlyingObservable(after: Long) =
        Task {
          lastErrorAt match {
            case -2 =>
              lastErrorAt = -1
              throw new IllegalArgumentException("GET-ERROR")
            case -1 =>
              lastErrorAt = 0
              Left(Problem("TEST-PROBLEM"))
            case _ =>
              Right(Observable.fromIterator(Task(
                Iterator.from(after.toInt + 1)
                  .map {
                    case 3 if lastErrorAt != 3 =>
                      lastErrorAt = 3
                      throw new IllegalArgumentException("TEST-ERROR")
                    case i => i
                  }.map(_.toString))))
          }
        }
      RecouplingStreamReader.observe[Long, String, TestSessionApi](
        toIndex = _.toLong,
        api,
        recouplingStreamReaderConf,
        after = 0L,
        getObservable = getUnderlyingObservable)
    }
    observable.take(10).toListL.timeout(99.s).runToFuture.flatMap { list =>
      assert(list == (1 to 10).map(_.toString).toList)

      // Cancel
      val obs = observable.doOnNext(_ => Task.sleep(10.ms))
        .toListL
        .onCancelRaiseError(new RuntimeException("TEST"))
        .runToFuture
      sleep(50.ms)
      obs.cancel()
      assert(Await.ready(obs, 99.s).value exists (_.isFailure))
    }
  }
}
