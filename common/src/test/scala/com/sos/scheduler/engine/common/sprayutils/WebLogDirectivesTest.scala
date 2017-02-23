package com.sos.scheduler.engine.common.sprayutils

import com.sos.scheduler.engine.base.exceptions.StandardPublicException
import com.sos.scheduler.engine.common.sprayutils.WebLogDirectives.handleErrorAndLog
import org.scalatest.FreeSpec
import spray.http.StatusCodes.{BadRequest, InternalServerError}
import spray.routing.Directives._
import spray.testkit.ScalatestRouteTest

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
          assert(entity.asString == "MESSAGE")
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
          assert(entity.asString endsWith "$anon$1: MESSAGE")
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
          assert(entity.asString == "java.lang.RuntimeException")
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
          assert(entity.asString == "PUBLIC MESSAGE")
        }
  }
}
