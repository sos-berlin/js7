package com.sos.jobscheduler.common.akkahttp.web.session

import cats.effect.Resource
import cats.syntax.flatMap._
import com.sos.jobscheduler.base.auth.{UserAndPassword, UserId}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.session.{HttpAutoRelogin, HttpSessionApi}
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.web.Uri
import com.sos.jobscheduler.common.http.AkkaHttpClient
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import monix.catnap.MVar
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.freespec.AnyFreeSpec

final class HttpAutoReloginTest extends AnyFreeSpec with SessionRouteTester
{
  protected implicit def scheduler = Scheduler.global

  "HttpAutoRelogin retryUntilReachable repeats body after server loss" in {
    val progress = MVar[Task].empty[String]().memoize
    @volatile var loopCounter = 0

    val apiTask = Resource.fromAutoCloseable(Task {
      new HttpSessionApi with HttpAutoRelogin with AkkaHttpClient {
        protected def scheduler = Scheduler.global
        protected val name = "HttpAutoReloginTest"
        def httpClient = this
        def sessionUri = Uri(s"$baseUri/session")
        val actorSystem = HttpAutoReloginTest.this.system
        def baseUri = server.localUri
        def uriPrefixPath = ""
        protected val userAndPassword = Some(UserAndPassword(UserId("A-USER"), SecretString("A-PASSWORD")))
        protected def keyStoreRef = None
        protected def trustStoreRef = None
      }
    }).use(api =>
      api.retryUntilReachable {
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
        Task.deferFuture(server.start()) >>
          mvar.take.map(o => assert(o == "After retryUntilReachable")) >>
          mvar.take.map(o => assert(o == "After requireAuthorizedAccess")) >>
          mvar.take.map(o => assert(o == "After retryUntilReachable")) >>
          mvar.take.map(o => assert(o == "After requireAuthorizedAccess")) >>
          mvar.take.flatTap(o => Task(assert(o == "FINISHED"))))
    val apiFuture = Task.parMap2(apiTask, serverTask)((a, s) => (a, s)).runToFuture

    assert(apiFuture.await(99.s) == ("FINISHED", "FINISHED"))
  }
}
