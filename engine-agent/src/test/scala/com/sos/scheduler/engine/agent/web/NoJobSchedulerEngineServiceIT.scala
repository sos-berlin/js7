package com.sos.scheduler.engine.agent.web

import akka.actor.ActorSystem
import com.sos.scheduler.engine.agent.Agent
import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.Futures._
import com.sos.scheduler.engine.common.time.ScalaTime._
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.client.pipelining._
import spray.http.HttpResponse
import spray.http.StatusCodes.NotFound
import spray.httpx.UnsuccessfulResponseException

/**
 * JS-1509 404 rejection when accessing Universal Agent as Classic Agent.
 *
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class NoJobSchedulerEngineServiceIT extends FreeSpec {

  "Access as Classic Agent is rejected with special message" in {
    implicit lazy val actorSystem = ActorSystem(getClass.getSimpleName)
    import actorSystem.dispatcher
    autoClosing(Agent.forTest()) { agent ⇒
      awaitResult(agent.start(), 5.s)
      val responseFuture = (sendReceive ~> unmarshal[HttpResponse]).apply(Post(s"${agent.localUri}/jobscheduler/engine/command", <TEST/>))
      val e = intercept[UnsuccessfulResponseException] { awaitResult(responseFuture, 5.s) }
      assert(e.response.status == NotFound)
      assert(e.response.entity.asString contains "Classic Agent")
    }
    actorSystem.shutdown()
  }
}
