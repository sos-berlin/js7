package js7.common.akkahttp.web.session

import cats.effect.Resource
import cats.syntax.flatMap._
import js7.base.auth.{UserAndPassword, UserId}
import js7.base.generic.SecretString
import js7.base.io.https.HttpsConfig
import js7.base.thread.Futures.implicits._
import js7.base.time.ScalaTime._
import js7.base.web.Uri
import js7.common.http.AkkaHttpClient
import js7.data.session.HttpSessionApi
import monix.catnap.MVar
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.freespec.AnyFreeSpec

final class HasUserAndPasswordTest extends AnyFreeSpec with SessionRouteTester
{
  protected implicit def scheduler = Scheduler.global

  "HasUserAndPassword retryUntilReachable repeats body after server loss" in {
    val progress = MVar[Task].empty[String]().memoize
    @volatile var loopCounter = 0

    val apiTask = Resource.fromAutoCloseable(Task {
      new HttpSessionApi with AkkaHttpClient {
        protected val name = "HasUserAndPasswordTest"
        def httpClient = this
        def sessionUri = Uri(s"$baseUri/session")
        val actorSystem = HasUserAndPasswordTest.this.system
        val baseUri = server.localUri
        def uriPrefixPath = ""
        protected val userAndPassword = Some(UserAndPassword(UserId("A-USER"), SecretString("A-PASSWORD")))
        protected def httpsConfig = HttpsConfig.empty
      }
    }).use(api =>
      api.retryUntilReachable() {
        import api.implicitSessionToken
        progress.flatMap(mvar =>
          Task.defer {
            loopCounter += 1
            if (loopCounter > 2)
              mvar.put("FINISHED")
                .map(_ => "FINISHED")
            else
              mvar.put("After retryUntilReachable") >>
              Task { requireAuthorizedAccess(api) } >>
              mvar.put("After requireAuthorizedAccess") >>
              api.get_[String](Uri(s"$localUri/ServiceUnavailable"))
                .map(_ => throw new UnsupportedOperationException)
          })
      })

    val serverTask = progress
      .flatMap(mvar =>
        mvar.tryTake.map(o => assert(o == None)) >>
          (server.start >>
            mvar.take.map(o => assert(o == "After retryUntilReachable")) >>
            mvar.take.map(o => assert(o == "After requireAuthorizedAccess")) >>
            mvar.take.map(o => assert(o == "After retryUntilReachable")) >>
            mvar.take.map(o => assert(o == "After requireAuthorizedAccess")) >>
            mvar.take.flatTap(o => Task(assert(o == "FINISHED")))
          ).guarantee(Task { server.close() }))
    val apiFuture = Task.parMap2(apiTask, serverTask)((a, s) => (a, s)).runToFuture

    assert(apiFuture.await(99.s) == ("FINISHED", "FINISHED"))
  }
}
