package com.sos.jobscheduler.common.akkahttp

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.StatusCodes.{BadRequest, Forbidden, InternalServerError}
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.sos.jobscheduler.base.exceptions.StandardPublicException
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.common.http.CirceJsonSupport._
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class WebLogDirectivesTest extends FreeSpec with ScalatestRouteTest with WebLogDirectives {

  protected def config = WebLogDirectives.TestConfig
  protected def actorSystem = system

  "RuntimeException" in {
    post("/") ~>
      handleErrorAndLog() {
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
      handleErrorAndLog() {
        complete {
          throw new RuntimeException("MESSAGE") {}
        }
      } ~>
        check {
          assert(status == InternalServerError)
          assert(entityAs[Problem] == Problem("com.sos.jobscheduler.common.akkahttp.WebLogDirectivesTest$$anon$1: MESSAGE"))
        }
  }

  "getMessage == null" in {
    post("/") ~>
      handleErrorAndLog() {
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
      handleErrorAndLog() {
        complete {
          throw new HttpStatusCodeException(Forbidden, Problem("PROBLEM"))
        }
      } ~>
        check {
          assert(status == Forbidden)
          assert(entityAs[Problem] == Problem("PROBLEM"))
        }
  }

  "PublicException" in {
    post("/") ~>
      handleErrorAndLog() {
        complete {
          throw new StandardPublicException("PUBLIC MESSAGE")
        }
      } ~>
        check {
          assert(status == BadRequest)
          assert(entityAs[Problem] == Problem("PUBLIC MESSAGE"))
        }
  }

  private def post(path: String) = Post("/") ~> Accept(`application/json`)
}
