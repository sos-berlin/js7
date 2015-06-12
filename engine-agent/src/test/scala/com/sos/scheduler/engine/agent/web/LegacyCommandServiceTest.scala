package com.sos.scheduler.engine.agent.web

import akka.actor.ActorSystem
import com.sos.scheduler.engine.agent.data.AgentProcessId
import com.sos.scheduler.engine.agent.data.commands._
import com.sos.scheduler.engine.agent.data.responses.{EmptyResponse, StartProcessResponse}
import com.sos.scheduler.engine.common.time.ScalaTime._
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import scala.concurrent.Future
import scala.xml.XML
import spray.http.StatusCodes.InternalServerError
import spray.testkit.ScalatestRouteTest

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class LegacyCommandServiceTest extends FreeSpec with ScalatestRouteTest with LegacyCommandService {

  implicit lazy val actorRefFactory = ActorSystem()

  protected def executeCommand(command: Command) =
    Future.successful {
      val expectedTerminate = Terminate(sigkillProcessesAfter = 999.s)
      command match {
        case StartSeparateProcess("0.0.0.0:999", "", "") ⇒ StartProcessResponse(AgentProcessId(123))
        case `expectedTerminate` ⇒ EmptyResponse
      }
    }

  "remote_scheduler.start_remote_task" in {
    post(<remote_scheduler.start_remote_task tcp_port='999'/>) ~> check {
      assert(XML.loadString(responseAs[String]) == <spooler><answer><process process_id="123"/></answer></spooler>)
    }
  }

  "Unknown XML command" in {
    post(<ERROR/>) ~> check {
      assert(status == InternalServerError)
    }
  }

  private def post(command: xml.Elem): RouteResult =
    Post("/jobscheduler/engine/command", command) ~>
      addHeader("Remote-Address", "0.0.0.0") ~>   // For this IP-less test only. Client's IP is normally set by configuration spray.can.remote-address-header
      legacyCommandRoute
}
