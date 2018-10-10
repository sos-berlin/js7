package com.sos.jobscheduler.agent.web

import akka.http.scaladsl.model.MediaTypes._
import akka.http.scaladsl.model.headers.Accept
import com.sos.jobscheduler.agent.web.test.WebServiceTest
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegments
import com.sos.jobscheduler.common.http.CirceJsonSupport._
import com.sos.jobscheduler.common.scalautil.Closer.ops.RichClosersAutoCloseable
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.{TimerOverview, TimerService, TimerServiceOverview}
import io.circe.Json
import java.time.Instant
import monix.execution.Scheduler
import org.scalatest.FreeSpec
import scala.collection.immutable

/**
  * @author Joacim Zschimmer
  */
final class TimerWebServiceTest extends FreeSpec with WebServiceTest with TimerWebService {

  protected def scheduler = Scheduler.global
  protected lazy val timerService = TimerService(Some(5.s)).closeWithCloser

  private val route =
    pathSegments("agent/api/timer") {
      timerRoute
    }

  "timerService (empty)" in {
    Get("/agent/api/timer") ~> testSessionHeader ~> Accept(`application/json`) ~> route ~> check {
      assert(responseAs[TimerServiceOverview] == timerService.overview)
      assert(responseAs[Json] == Json.obj(
        "count" → Json.fromInt(0),
        "completeCount" → Json.fromInt(0),
        "wakeCount" → Json.fromInt(0)
      ))
    }
  }

  "timerService/ (empty)" in {
    Get("/agent/api/timer/") ~> testSessionHeader ~> Accept(`application/json`) ~> route ~> check {
      assert(responseAs[immutable.Seq[TimerOverview]] == timerService.timerOverviews)
      assert(responseAs[Json] == Json.fromValues(Nil))
    }
  }

  "timerService (3 timers)" in {
    timerService.at(Instant.parse("2111-01-01T12:11:11Z"), name = "TEST-A")
    timerService.at(Instant.parse("2222-01-02T12:22:22Z"), name = "TEST-B")
    timerService.at(Instant.parse("2333-01-03T12:33:33Z"), name = "TEST-C")
    Get("/agent/api/timer") ~> testSessionHeader ~> Accept(`application/json`) ~> route ~> check {
      assert(responseAs[TimerServiceOverview] == timerService.overview)
      assert(responseAs[Json] == Json.obj(
        "count" → Json.fromInt(3),
        "completeCount" → Json.fromInt(0),
        "wakeCount" → Json.fromInt(0),
        "first" → Json.obj(
          "at" → Json.fromString("2111-01-01T12:11:11Z"),
          "name" → Json.fromString("TEST-A")),
        "last" → Json.obj(
          "at" → Json.fromString("2333-01-03T12:33:33Z"),
          "name" → Json.fromString("TEST-C"))))
    }
  }

  "timerService/ (3 timers)" in {
    Get("/agent/api/timer/") ~> testSessionHeader ~> Accept(`application/json`) ~> route ~> check {
      assert(responseAs[immutable.Seq[TimerOverview]] == timerService.timerOverviews)
      assert(responseAs[Json] == Json.fromValues(List(
        Json.obj(
          "at" → Json.fromString("2111-01-01T12:11:11Z"),
          "name" → Json.fromString("TEST-A")),
        Json.obj(
          "at" → Json.fromString("2222-01-02T12:22:22Z"),
          "name" → Json.fromString("TEST-B")),
        Json.obj(
          "at" → Json.fromString("2333-01-03T12:33:33Z"),
          "name" → Json.fromString("TEST-C")))))
    }
  }
}
