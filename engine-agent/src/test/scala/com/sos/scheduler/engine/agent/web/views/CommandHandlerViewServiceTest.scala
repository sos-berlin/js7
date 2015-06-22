package com.sos.scheduler.engine.agent.web.views

import akka.actor.ActorSystem
import com.sos.scheduler.engine.agent.command.{CommandHandlerDetails, CommandHandlerOverview, CommandRunOverview, InternalCommandId}
import com.sos.scheduler.engine.agent.data.commands.{Command, Terminate}
import com.sos.scheduler.engine.agent.web.marshal.JsObjectMarshallers._
import java.time.Instant
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes.`application/json`
import spray.http.Uri
import spray.json._
import spray.testkit.ScalatestRouteTest


/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class CommandHandlerViewServiceTest extends FreeSpec with ScalatestRouteTest with CommandHandlerViewService {

  implicit lazy val actorRefFactory = ActorSystem()

  protected def commandHandlerOverview = new CommandHandlerOverview {
    def totalCommandCount = 1111
    def currentCommandCount = 2222
  }

  private val testCommand = Terminate(sigtermProcesses = false)

  protected def commandHandlerDetails = new CommandHandlerDetails {
    def commandRuns = List(CommandRunOverview(InternalCommandId(3333), Instant.parse("2015-06-22T12:00:00Z"), testCommand))
  }

  "commandHandler" in {
    Get(Uri("/jobscheduler/agent/commandHandler")) ~> Accept(`application/json`) ~> route ~> check {
      assert(responseAs[JsObject] == JsObject(
        "totalCommandCount" → JsNumber(1111),
        "currentCommandCount" → JsNumber(2222)))
    }
  }

  "commandHandler/details" in {
    Get(Uri("/jobscheduler/agent/commandHandler/details")) ~> Accept(`application/json`) ~> route ~> check {
      assert(responseAs[JsObject] == JsObject(
        "commandRuns" → JsArray(
          JsObject(
            "internalId" → JsString("3333"),
            "startedAt" → JsString("2015-06-22T12:00:00Z"),
            "command" → (testCommand: Command).toJson))))
    }
  }
}
