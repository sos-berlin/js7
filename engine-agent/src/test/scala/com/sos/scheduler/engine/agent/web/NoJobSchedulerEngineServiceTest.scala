package com.sos.scheduler.engine.agent.web

import akka.actor.ActorSystem
import org.scalatest.FreeSpec
import spray.http.StatusCodes.NotFound
import spray.http.Uri
import spray.testkit.ScalatestRouteTest

/**
 * @author Joacim Zschimmer
 */
final class NoJobSchedulerEngineServiceTest extends FreeSpec with ScalatestRouteTest with NoJobSchedulerEngineService {

  protected implicit lazy val actorRefFactory = ActorSystem()

  "/jobscheduler/engine/command" in {
    Post(Uri("/jobscheduler/engine/command"), <test/>) ~> route ~> check {
      assert(status == NotFound)
      assert(entity.asString contains NoJobSchedulerEngineService.Message)
    }
  }

  "/jobscheduler/engine" in {
    Get(Uri("/jobscheduler/engine")) ~> route ~> check {
      assert(status == NotFound)
      assert(entity.asString contains NoJobSchedulerEngineService.Message)
    }
  }
}
