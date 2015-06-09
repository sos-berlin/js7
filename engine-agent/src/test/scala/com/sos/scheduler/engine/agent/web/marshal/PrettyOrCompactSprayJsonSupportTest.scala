package com.sos.scheduler.engine.agent.web.marshal

import akka.actor.ActorSystem
import com.sos.scheduler.engine.agent.web.marshal.PrettyOrCompactSprayJsonSupport._
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes._
import spray.http.{ContentType, ContentTypes, Uri}
import spray.json.DefaultJsonProtocol._
import spray.json.{JsObject, JsString, _}
import spray.routing.HttpService
import spray.testkit.ScalatestRouteTest

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class PrettyOrCompactSprayJsonSupportTest extends FreeSpec with ScalatestRouteTest with HttpService {

  implicit lazy val actorRefFactory = ActorSystem()

  private val expectedJsObject = JsObject("test" → JsString("TEST"))
  private def route = complete { expectedJsObject }

  "Response is compact, application/json " - {
    "Accept: application/json" in {
      Get(Uri("/")) ~> Accept(`application/json`) ~> route ~> check {
        assert(contentType == ContentTypes.`application/json`)
        assert(responseAs[JsObject] == expectedJsObject)
        assert(!(responseAs[String] contains " "))
      }
    }

    "Accept: application/json, text/plain;q=0.5" in {
      Get(Uri("/")) ~> Accept(`application/json`, `text/plain` withQValue 0.5) ~> route ~> check {
        assert(contentType == ContentTypes.`application/json`)
        assert(responseAs[JsObject] == expectedJsObject)
        assert(!(responseAs[String] contains " "))
      }
    }
  }

  "Response is pretty, text/plain" - {
    "Accept: text/plain" in {
      Get(Uri("/")) ~> Accept(`text/plain`) ~> route ~> check {
        requirePrettyTextPlain(contentType, responseAs[String])
      }
    }

    "Accept: application/json, text/plain" in {
      Get(Uri("/")) ~> Accept(`application/json`, `text/plain`) ~> route ~> check {
        requirePrettyTextPlain(contentType, responseAs[String])
      }
    }

    def requirePrettyTextPlain(contentType: ContentType, string: String): Unit = {
      assert(contentType == ContentTypes.`text/plain(UTF-8)`)
      assert(JsonParser(string) == expectedJsObject)
      assert(string contains " ")
    }
  }
}
