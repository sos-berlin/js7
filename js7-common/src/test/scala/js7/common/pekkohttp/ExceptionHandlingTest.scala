package js7.common.pekkohttp

import cats.effect.Deferred
import js7.base.configutils.Configs.*
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.common.pekkohttp.CirceJsonSupport.jsonUnmarshaller
import js7.common.pekkohttp.ExceptionHandlingTest.*
import org.apache.pekko.http.scaladsl.model.MediaTypes.`application/json`
import org.apache.pekko.http.scaladsl.model.StatusCodes.{Forbidden, InternalServerError}
import org.apache.pekko.http.scaladsl.model.headers.Accept
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.testkit.ScalatestRouteTest
import scala.util.control.NoStackTrace

/**
  * @author Joacim Zschimmer
  */
final class ExceptionHandlingTest
extends OurTestSuite, ScalatestRouteTest, ExceptionHandling:

  protected val config = config"js7.web.server.verbose-error-messages = true"
  protected val whenShuttingDown = Deferred.unsafe

  override def testConfig = config"pekko.loglevel = warning"
    .withFallback(super.testConfig)

  protected def actorSystem = system

  "RuntimeException" in:
    post("/") ~>
      seal {
        complete:
          throw new RuntimeException("MESSAGE")
      } ~>
        check:
          assert(status == InternalServerError)
          assert(entityAs[Problem] == Problem("MESSAGE"))

  "Any exception" in:
    post("/") ~>
      seal {
        complete:
          throw new TestException("MESSAGE")
      } ~>
        check:
          assert(status == InternalServerError)
          assert(entityAs[Problem] == Problem("js7.common.pekkohttp.ExceptionHandlingTest$TestException: MESSAGE"))

  "getMessage == null" in:
    post("/") ~>
      seal {
        complete:
          throw new RuntimeException
      } ~>
        check:
          assert(status == InternalServerError)
          assert(entityAs[Problem] == Problem("java.lang.RuntimeException"))

  "HttpStatusCodeException" in:
    post("/") ~>
      seal {
        complete:
          throw new HttpStatusCodeException(Forbidden, Problem("PROBLEM"))
      } ~>
        check:
          assert(status == Forbidden)
          assert(entityAs[Problem] == Problem("PROBLEM"))

  private def post(path: String) = Post("/") ~> Accept(`application/json`)


object ExceptionHandlingTest:
  private class TestException(message: String) extends RuntimeException(message), NoStackTrace
