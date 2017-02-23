package com.sos.scheduler.engine.agent.web

import com.sos.scheduler.engine.agent.web.test.WebServiceTest
import org.scalatest.FreeSpec
import spray.http.StatusCodes.NotFound

/**
 * @author Joacim Zschimmer
 */
final class NoJobSchedulerEngineWebServiceTest extends FreeSpec with WebServiceTest with NoJobSchedulerEngineWebService {

  "/jobscheduler/engine/command" in {
    Post("/jobscheduler/engine/command", <test/>) ~> route ~> check {
      assert(status == NotFound)
      assert(entity.asString contains NoJobSchedulerEngineWebService.Message)
    }
  }

  "/jobscheduler/engine" in {
    Get("/jobscheduler/engine") ~> route ~> check {
      assert(status == NotFound)
      assert(entity.asString contains NoJobSchedulerEngineWebService.Message)
    }
  }
}
