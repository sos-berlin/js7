package com.sos.jobscheduler.common.akkahttp

import akka.http.scaladsl.model.StatusCodes.{BadRequest, InternalServerError}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.sos.jobscheduler.base.exceptions.StandardPublicException
import com.sos.jobscheduler.common.akkahttp.WebLogDirectives.handleErrorAndLog
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class WebLogDirectivesTest extends FreeSpec with ScalatestRouteTest {

  "RuntimeException" in {
    Post("/") ~>
      handleErrorAndLog(WebLogDirectives.TestConfig, system).apply {
        complete {
          throw new RuntimeException("MESSAGE")
        }
      } ~>
        check {
          assert(status == InternalServerError)
          assert(entityAs[String] == "MESSAGE\n")
        }
  }

  "Any exception" in {
    Post("/") ~>
      handleErrorAndLog(WebLogDirectives.TestConfig, system).apply {
        complete { throw new RuntimeException("MESSAGE") {}
        }
      } ~>
        check {
          assert(status == InternalServerError)
          assert(entityAs[String] endsWith "$anon$1: MESSAGE\n")
        }
  }

  "getMessage == null" in {
    Post("/") ~>
      handleErrorAndLog(WebLogDirectives.TestConfig, system).apply {
        complete {
          throw new RuntimeException
        }
      } ~>
        check {
          assert(status == InternalServerError)
          assert(entityAs[String] == "java.lang.RuntimeException\n")
        }
  }

  "PublicException" in {
    Post("/") ~>
      handleErrorAndLog(WebLogDirectives.TestConfig, system).apply {
        complete { throw new StandardPublicException("PUBLIC MESSAGE")
        }
      } ~>
        check {
          assert(status == BadRequest)
          assert(entityAs[String] == "PUBLIC MESSAGE\n")
        }
  }
}
