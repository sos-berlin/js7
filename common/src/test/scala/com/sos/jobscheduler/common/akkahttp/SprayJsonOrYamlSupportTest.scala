package com.sos.jobscheduler.common.akkahttp

import akka.actor.ActorSystem
import akka.http.scaladsl.model.MediaTypes._
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.model.{ContentTypes, MediaRanges}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.sos.jobscheduler.common.akkahttp.SprayJsonOrYamlSupport._
import org.scalatest.FreeSpec
import spray.json.DefaultJsonProtocol._
import spray.json.{JsObject, JsString}

/**
 * @author Joacim Zschimmer
 */
final class SprayJsonOrYamlSupportTest extends FreeSpec with ScalatestRouteTest {

  private implicit lazy val actorSystem = ActorSystem(getClass.getSimpleName)

  private val expectedJsObject = JsObject("test" → JsString("TEST"))
  private val expectedYaml = "test: TEST\n"

  private def route = complete { expectedJsObject }

  override protected def afterAll() = {
    actorSystem.terminate()
    super.afterAll()
  }

  "Response is compact, application/json " - {
    "Accept: application/json" in {
      Get("/") ~> Accept(`application/json`) ~> route ~> check {
        requireJson()
      }
    }

    "Accept: application/json, text/plain;q=0.5" in {
      Get("/") ~> Accept(`application/json`, `text/plain` withQValue 0.5) ~> route ~> check {
        requireJson()
      }
    }

    "Accept: text/plain;q=0.5, application/json" in {
      Get("/") ~> Accept(`text/plain` withQValue 0.5, `application/json`) ~> route ~> check {
        requireJson()
      }
    }

    def requireJson() = {
      assert(contentType == ContentTypes.`application/json`)
      assert(responseAs[JsObject] == expectedJsObject)
      assert(!(responseAs[String] contains " "))
    }
  }

  "Response is pretty, text/plain YAML" - {
    "Without Accept" in {
      Get("/") ~> route ~> check {
        requireYaml()
      }
    }

    "Accept: */*" in {
      Get("/") ~> Accept(MediaRanges.`*/*`) ~> route ~> check {
        requireYaml()
      }
    }

    "Accept: text/plain" in {
      Get("/") ~> Accept(`text/plain`) ~> route ~> check {
        requireYaml()
      }
    }

    "Accept: text/plain, application/json" in {
      Get("/") ~> Accept(`text/plain`, `application/json`) ~> route ~> check {
        requireYaml()
      }
    }

    "Accept: application/json, text/plain" in {
      Get("/") ~> Accept(`application/json`, `text/plain`) ~> route ~> check {
        requireYaml()
      }
    }

    def requireYaml() = {
      assert(contentType == ContentTypes.`text/plain(UTF-8)`)
      assert(responseAs[String] == expectedYaml)
    }
  }
}
