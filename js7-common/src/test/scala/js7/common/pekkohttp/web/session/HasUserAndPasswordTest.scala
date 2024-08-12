package js7.common.pekkohttp.web.session

import cats.effect.{IO, Resource}
import js7.base.auth.{UserAndPassword, UserId}
import js7.base.catsutils.UnsafeMemoizable.unsafeMemoize
import js7.base.generic.SecretString
import js7.base.io.https.HttpsConfig
import js7.base.session.SessionApi
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.base.utils.MVar
import js7.base.web.Uri
import js7.common.http.PekkoHttpClient
import js7.data.session.HttpSessionApi

final class HasUserAndPasswordTest extends OurAsyncTestSuite, SessionRouteTester:

  "HasUserAndPassword retryUntilReachable repeats body after server loss" in:
    val progress = MVar[IO].empty[String].unsafeMemoize
    @volatile var loopCounter = 0

    val apiIO = Resource
      .fromAutoCloseable(IO:
        new HttpSessionApi with PekkoHttpClient with SessionApi.HasUserAndPassword:
          protected val name = "HasUserAndPasswordTest"
          def httpClient = this
          val baseUri = allocatedWebServer.allocatedThing.localUri
          val sessionUri = Uri(s"$baseUri/session")
          val actorSystem = HasUserAndPasswordTest.this.system
          def uriPrefixPath = ""
          protected val userAndPassword = Some:
            UserAndPassword(UserId("A-USER"), SecretString("A-PASSWORD"))
          protected def httpsConfig = HttpsConfig.empty)
      .use: api =>
        api.retryUntilReachable():
          import api.implicitSessionToken
          progress.flatMap: mvar =>
            IO.defer:
              loopCounter += 1
              if loopCounter > 2 then
                mvar.put("FINISHED").as("FINISHED")
              else
                mvar.put("After retryUntilReachable") >>
                IO(requireAuthorizedAccess(api)) >>
                mvar.put("After requireAuthorizedAccess") >>
                api.get_[String](Uri(s"$localUri/ServiceUnavailable"))
                  .map(_ => throw new UnsupportedOperationException)

    val serverIO = progress
      .flatMap: mvar =>
        //time dependant: mvar.tryTake.map(o => assert(o == None)) >>
          (IO(allocatedWebServer) >>
            mvar.take.map(o => assert(o == "After retryUntilReachable")) >>
            mvar.take.map(o => assert(o == "After requireAuthorizedAccess")) >>
            mvar.take.map(o => assert(o == "After retryUntilReachable")) >>
            mvar.take.map(o => assert(o == "After requireAuthorizedAccess")) >>
            mvar.take.flatTap(o => IO(assert(o == "FINISHED")))
          ).guarantee(allocatedWebServer.release)

    IO.both(apiIO, serverIO)
      .map: result =>
        assert(result == ("FINISHED", "FINISHED"))
