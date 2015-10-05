package com.sos.scheduler.engine.agent.web.views

import akka.actor.ActorSystem
import com.sos.scheduler.engine.agent.command.{CommandHandlerOverview, CommandRunOverview, InternalCommandId}
import com.sos.scheduler.engine.agent.data.commands.{Command, Terminate}
import com.sos.scheduler.engine.common.sprayutils.JsArrayMarshallers._
import java.time.Instant
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes.`application/json`
import spray.http.Uri
import spray.httpx.SprayJsonSupport._
import spray.json.DefaultJsonProtocol._
import spray.json._
import spray.testkit.ScalatestRouteTest

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class CommandHandlerViewServiceTest extends FreeSpec with ScalatestRouteTest with CommandHandlerViewService {

  protected implicit lazy val actorRefFactory = ActorSystem()
  override protected val uriPathPrefix = "test"

  protected def commandHandlerOverview = new CommandHandlerOverview {
    def totalCommandCount = 1111
    def currentCommandCount = 2222
  }

  private val testCommand = Terminate(sigtermProcesses = false)

  protected def commandRunOverviews = List(CommandRunOverview(InternalCommandId(3333), Instant.parse("2015-06-22T12:00:00Z"), testCommand))

  "commandHandler returns overview" in {
    Get(Uri("/test/jobscheduler/agent/api/command")) ~> Accept(`application/json`) ~> route ~> check {
      assert(responseAs[JsObject] == JsObject(
        "totalCommandCount" → JsNumber(1111),
        "currentCommandCount" → JsNumber(2222)))
    }
  }

  "commandHandler/ returns array of running command" in {
    Get(Uri("/test/jobscheduler/agent/api/command/")) ~> Accept(`application/json`) ~> route ~> check {
      assert(responseAs[JsArray] == JsArray(
        JsObject(
          "internalId" → JsString("3333"),
          "startedAt" → JsString("2015-06-22T12:00:00Z"),
          "command" → (testCommand: Command).toJson)))
    }
  }
}
