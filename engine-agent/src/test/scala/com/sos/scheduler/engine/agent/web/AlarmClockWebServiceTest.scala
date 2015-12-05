package com.sos.scheduler.engine.agent.web

import akka.actor.ActorSystem
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.alarm.{AlarmClock, AlarmClockOverview, AlarmOverview}
import java.time.Instant
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.collection.immutable
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes._
import spray.http.Uri
import spray.httpx.SprayJsonSupport._
import spray.json.DefaultJsonProtocol._
import spray.json.{JsArray, JsObject, _}
import spray.testkit.ScalatestRouteTest

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class AlarmClockWebServiceTest extends FreeSpec with BeforeAndAfterAll with ScalatestRouteTest with AlarmClockWebService  {

  protected implicit lazy val actorRefFactory = ActorSystem()
  protected lazy val alarmClock = new AlarmClock(1.s)

  override protected def afterAll() = {
    alarmClock.close()
    super.afterAll()
  }

  "alarmClock (empty)" in {
    Get(Uri("/jobscheduler/agent/api/alarmClock")) ~> Accept(`application/json`) ~> route ~> check {
      assert(responseAs[AlarmClockOverview] == alarmClock.overview)
      assert(responseAs[JsObject] == JsObject("count" → JsNumber(0)))
    }
  }

  "alarmClock/ (empty)" in {
    Get(Uri("/jobscheduler/agent/api/alarmClock/")) ~> Accept(`application/json`) ~> route ~> check {
      assert(responseAs[immutable.Seq[AlarmOverview]] == alarmClock.alarmOverviews)
      assert(responseAs[JsArray] == JsArray())
    }
  }

  "alarmClock (3 alarms)" in {
    alarmClock.at(Instant.parse("2111-01-01T12:11:11Z"), name = "TEST-A") {}
    alarmClock.at(Instant.parse("2222-01-02T12:22:22Z"), name = "TEST-B") {}
    alarmClock.at(Instant.parse("2333-01-03T12:33:33Z"), name = "TEST-C") {}
    Get(Uri("/jobscheduler/agent/api/alarmClock")) ~> Accept(`application/json`) ~> route ~> check {
      assert(responseAs[AlarmClockOverview] == alarmClock.overview)
      assert(responseAs[JsObject] == JsObject(
        "count" → JsNumber(3),
        "first" → JsObject(
          "at" → JsString("2111-01-01T12:11:11Z"),
          "name" → JsString("TEST-A")),
        "last" → JsObject(
          "at" → JsString("2333-01-03T12:33:33Z"),
          "name" → JsString("TEST-C"))))
    }
  }

  "alarmClock/ (3 alarms)" in {
    Get(Uri("/jobscheduler/agent/api/alarmClock/")) ~> Accept(`application/json`) ~> route ~> check {
      assert(responseAs[immutable.Seq[AlarmOverview]] == alarmClock.alarmOverviews)
      assert(responseAs[JsArray] == JsArray(
        JsObject(
          "at" → JsString("2111-01-01T12:11:11Z"),
          "name" → JsString("TEST-A")),
        JsObject(
          "at" → JsString("2222-01-02T12:22:22Z"),
          "name" → JsString("TEST-B")),
        JsObject(
          "at" → JsString("2333-01-03T12:33:33Z"),
          "name" → JsString("TEST-C"))))
    }
  }

}
