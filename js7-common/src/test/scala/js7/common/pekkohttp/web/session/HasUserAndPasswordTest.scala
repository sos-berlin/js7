package js7.common.pekkohttp.web.session

import cats.effect.Resource
import cats.syntax.flatMap.*
import js7.base.auth.{UserAndPassword, UserId}
import js7.base.generic.SecretString
import js7.base.io.https.HttpsConfig
import js7.base.session.SessionApi
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.*
import js7.base.time.ScalaTime.*
import js7.base.web.Uri
import js7.common.http.PekkoHttpClient
import js7.data.session.HttpSessionApi
import monix.catnap.MVar
import monix.eval.Task
import monix.execution.Scheduler

final class HasUserAndPasswordTest extends OurTestSuite with SessionRouteTester:
  protected implicit def scheduler = Scheduler.traced

  "HasUserAndPassword retryUntilReachable repeats body after server loss" in:
    val progress = MVar[Task].empty[String]().memoize
    @volatile var loopCounter = 0

    val apiTask = Resource.fromAutoCloseable(Task {
      new HttpSessionApi with PekkoHttpClient with SessionApi.HasUserAndPassword {
        protected val name = "HasUserAndPasswordTest"
        def httpClient = this
        def sessionUri = Uri(s"$baseUri/session")
        val actorSystem = HasUserAndPasswordTest.this.system
        val baseUri = allocatedWebServer.allocatedThing.localUri
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
            if loopCounter > 2 then
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
          (Task(allocatedWebServer) >>
            mvar.take.map(o => assert(o == "After retryUntilReachable")) >>
            mvar.take.map(o => assert(o == "After requireAuthorizedAccess")) >>
            mvar.take.map(o => assert(o == "After retryUntilReachable")) >>
            mvar.take.map(o => assert(o == "After requireAuthorizedAccess")) >>
            mvar.take.flatTap(o => Task(assert(o == "FINISHED")))
          ).guarantee(allocatedWebServer.release))
    val apiFuture = Task.parMap2(apiTask, serverTask)((a, s) => (a, s)).runToFuture

    assert(apiFuture.await(99.s) == ("FINISHED", "FINISHED"))
