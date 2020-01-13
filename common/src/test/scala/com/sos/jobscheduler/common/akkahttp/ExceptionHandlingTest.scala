package com.sos.jobscheduler.common.akkahttp

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.StatusCodes.{Forbidden, InternalServerError}
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.common.akkahttp.ExceptionHandlingTest._
import com.sos.jobscheduler.common.http.CirceJsonSupport._
import com.typesafe.config.ConfigFactory
import org.scalatest.FreeSpec
import scala.util.control.NoStackTrace

/**
  * @author Joacim Zschimmer
  */
final class ExceptionHandlingTest extends FreeSpec with ScalatestRouteTest with ExceptionHandling
{
  protected val config = ConfigFactory.parseString("jobscheduler.webserver.verbose-error-messages = true")

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
          assert(entityAs[Problem] == Problem("com.sos.jobscheduler.common.akkahttp.ExceptionHandlingTest$TestException: MESSAGE"))
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
