package com.sos.scheduler.engine.agent.web.common

import com.sos.scheduler.engine.base.exceptions.StandardPublicException
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.http.StatusCodes.{BadRequest, InternalServerError}
import spray.routing.Directives._
import spray.testkit.ScalatestRouteTest

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class AgentExceptionHandlerTest extends FreeSpec with ScalatestRouteTest with AgentExceptionHandler {

  "RuntimeException" in {
    Post("/") ~> complete { throw new RuntimeException("MESSAGE") } ~>
      check {
        assert(status == InternalServerError)
        assert(entity.asString == "MESSAGE")
      }
  }

  "Any exception" in {
    Post("/") ~> complete { throw new RuntimeException("MESSAGE") {} } ~>
      check {
        assert(status == InternalServerError)
        assert(entity.asString endsWith "$anon$1: MESSAGE")
      }
  }

  "getMessage == null" in {
    Post("/") ~> complete { throw new RuntimeException } ~>
      check {
        assert(status == InternalServerError)
        assert(entity.asString == "java.lang.RuntimeException")
      }
  }

  "PublicException" in {
    Post("/") ~> complete { throw new StandardPublicException("PUBLIC MESSAGE") } ~>
      check {
        assert(status == BadRequest)
        assert(entity.asString == "PUBLIC MESSAGE")
      }
  }
}
