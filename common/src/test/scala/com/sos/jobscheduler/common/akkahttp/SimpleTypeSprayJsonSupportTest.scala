package com.sos.jobscheduler.common.akkahttp

import akka.actor.ActorSystem
import akka.http.scaladsl.model.ContentTypes
import akka.http.scaladsl.model.MediaTypes._
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.sos.jobscheduler.common.akkahttp.SimpleTypeSprayJsonSupport._
import org.scalatest.FreeSpec
import spray.json._

/**
 * @author Joacim Zschimmer
 */
final class SimpleTypeSprayJsonSupportTest extends FreeSpec with ScalatestRouteTest {

  implicit lazy val actorRefFactory = ActorSystem(getClass.getSimpleName)

  override def afterAll(): Unit = {
    actorRefFactory.terminate()
    super.afterAll()
  }

  private val TestBoolean: JsBoolean = JsTrue
  private val TestInt = JsNumber(123456789)
  private val TestLong = JsNumber(123456789012345678L)
  private val TestBigDecimal = JsNumber(BigDecimal("111222333444555666777888999000.111222333"))
  private val TestString = JsString("TEST")

  private def route =
    path("Boolean") { complete { TestBoolean } } ~
    path("Int") { complete { TestInt } } ~
    path("Long") { complete { TestLong } } ~
    path("BigDecimal") { complete { TestBigDecimal } } ~
    path("String") { complete { TestString } }

  "Boolean" in {
    Get("/Boolean") ~> Accept(`application/json`) ~> route ~> check {
      assert(contentType == ContentTypes.`application/json`)
      assert(responseAs[JsBoolean] == JsTrue)
    }
  }

  "Int" in {
    Get("/Int") ~> Accept(`application/json`) ~> route ~> check {
      assert(contentType == ContentTypes.`application/json`)
      assert(responseAs[JsNumber] == TestInt)
    }
  }

  "Long" in {
    Get("/Long") ~> Accept(`application/json`) ~> route ~> check {
      assert(contentType == ContentTypes.`application/json`)
      assert(responseAs[JsNumber] == TestLong)
    }
  }

  "BigDecimal" in {
    Get("/BigDecimal") ~> Accept(`application/json`) ~> route ~> check {
      assert(contentType == ContentTypes.`application/json`)
      assert(responseAs[JsNumber] == TestBigDecimal)
    }
  }

  "String" in {
    Get("/String") ~> Accept(`application/json`) ~> route ~> check {
      assert(contentType == ContentTypes.`application/json`)
      assert(responseAs[JsString] == TestString)
    }
  }
}
