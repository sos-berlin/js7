package com.sos.scheduler.engine.agent.web

import com.sos.scheduler.engine.agent.configuration.Akkas._
import com.sos.scheduler.engine.common.scalautil.HasCloser
import java.net.InetAddress
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import scala.concurrent.Future
import spray.http.StatusCodes.InternalServerError
import spray.testkit.ScalatestRouteTest

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class AgentWebServiceTest extends FreeSpec with ScalatestRouteTest with AgentWebService with HasCloser {

  implicit lazy val actorRefFactory = newActorSystem("TEST")(closer)

  "Good command" in {
    postCommand("test") ~> check {
      responseAs[String] shouldEqual "<ok/>"
    }
  }

  "Exception" in {
    postCommand("error") ~> check {
      assert(status == InternalServerError)
    }
  }

  private def postCommand(command: String): RouteResult =
    Post("/jobscheduler/engine/command", command) ~>
      addHeader("Remote-Address", "0.0.0.0") ~>   // For this IP-less test only. Client's IP is normally set by configuration spray.can.remote-address-header
      route

  protected def executeCommand(clientIP: InetAddress, command: String) = Future.successful[xml.Elem](
    command match {
      case "test" ⇒ <ok/>
      case _ ⇒ throw new Exception
    })
}
