package com.sos.scheduler.engine.agent.web

import com.sos.scheduler.engine.agent.web.test.WebServiceTest
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.http.StatusCodes.NotFound
import spray.http.Uri

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class NoJobSchedulerEngineWebServiceTest extends FreeSpec with WebServiceTest with NoJobSchedulerEngineWebService {

  "/jobscheduler/engine/command" in {
    Post(Uri("/jobscheduler/engine/command"), <test/>) ~> route ~> check {
      assert(status == NotFound)
      assert(entity.asString contains NoJobSchedulerEngineWebService.Message)
    }
  }

  "/jobscheduler/engine" in {
    Get(Uri("/jobscheduler/engine")) ~> route ~> check {
      assert(status == NotFound)
      assert(entity.asString contains NoJobSchedulerEngineWebService.Message)
    }
  }
}
