package com.sos.scheduler.engine.agent.web

import akka.actor.ActorSystem
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.http.StatusCodes.NotFound
import spray.http.Uri
import spray.testkit.ScalatestRouteTest

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class NoJobSchedulerEngineWebServiceTest extends FreeSpec with ScalatestRouteTest with NoJobSchedulerEngineWebService {

  protected implicit lazy val actorRefFactory = ActorSystem()

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
