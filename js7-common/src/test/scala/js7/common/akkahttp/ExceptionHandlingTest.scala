package js7.common.akkahttp

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.StatusCodes.{Forbidden, InternalServerError}
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import js7.base.configutils.Configs._
import js7.base.problem.Problem
import js7.common.akkahttp.CirceJsonSupport.jsonUnmarshaller
import js7.common.akkahttp.ExceptionHandlingTest._
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.Future
import scala.util.control.NoStackTrace

/**
  * @author Joacim Zschimmer
  */
final class ExceptionHandlingTest extends AnyFreeSpec with ScalatestRouteTest with ExceptionHandling
{
  protected val config = config"js7.web.server.verbose-error-messages = true"
  protected def whenShuttingDown = Future.never

  protected def actorSystem = system

  "RuntimeException" in {
    post("/") ~>
      seal {
        complete {
          throw new RuntimeException("MESSAGE")
        }
      } ~>
        check {
          assert(status == InternalServerError)
          assert(entityAs[Problem] == Problem("MESSAGE"))
        }
  }

  "Any exception" in {
    post("/") ~>
      seal {
        complete {
          throw new TestException("MESSAGE")
        }
      } ~>
        check {
          assert(status == InternalServerError)
          assert(entityAs[Problem] == Problem("js7.common.akkahttp.ExceptionHandlingTest$TestException: MESSAGE"))
        }
  }

  "getMessage == null" in {
    post("/") ~>
      seal {
        complete {
          throw new RuntimeException
        }
      } ~>
        check {
          assert(status == InternalServerError)
          assert(entityAs[Problem] == Problem("java.lang.RuntimeException"))
        }
  }

  "HttpStatusCodeException" in {
    post("/") ~>
      seal {
        complete {
          throw new HttpStatusCodeException(Forbidden, Problem("PROBLEM"))
        }
      } ~>
        check {
          assert(status == Forbidden)
          assert(entityAs[Problem] == Problem("PROBLEM"))
        }
  }

  private def post(path: String) = Post("/") ~> Accept(`application/json`)
}

object ExceptionHandlingTest
{
  private class TestException(message: String) extends RuntimeException(message) with NoStackTrace
}
