package com.sos.jobscheduler.common.akkahttp

import akka.NotUsed
import akka.http.scaladsl.model.StatusCodes.{OK, TooManyRequests}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCode}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.stream.scaladsl.Source
import akka.util.ByteString
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.time.Timestamp.now
import com.sos.jobscheduler.common.akkahttp.StreamingSupport.JsonSeqStreamSupport
import com.sos.jobscheduler.common.http.CirceJsonSeqSupport.jsonSeqMarshaller
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.WaitForCondition.waitForCondition
import io.circe.Json
import org.scalatest.FreeSpec
import scala.concurrent.duration._
import scala.concurrent.{Future, blocking}

/**
  * @author Joacim Zschimmer
  */
final class ConcurrentRequestsLimiterExclusiveTest extends FreeSpec with ScalatestRouteTest
{
  private val concurrentProblem = Problem.pure("CONCURRENT")
  private val limiter = new ConcurrentRequestsLimiter(limit = 1, concurrentProblem)
  private val testResponse = HttpEntity.Strict(ContentTypes.`text/plain(UTF-8)`, ByteString("RESPONSE"))

  private def sleepingRoute(duration: Duration): Route = complete {
    Future {
      blocking {
        Thread.sleep(duration.toMillis)
        testResponse
      }
    }
  }

  "One request" in {
    executeRequest(0.millis, OK)
    executeRequest(0.millis, OK)
    executeRequest(0.millis, OK)
  }

  "limit=0 reject all (also warm-up for next test)" in {
    val zeroLimiter = new ConcurrentRequestsLimiter(limit = 0, concurrentProblem)
    Get() ~> zeroLimiter(sleepingRoute(0.millis)) ~> check {
      assert(status == TooManyRequests)
    }
  }

  "Two concurrent request" in {
    val a = Future { blocking { executeRequest(100.millis, OK) } }
    waitForCondition(1.s, 1.ms)(limiter.isBusy)
    assert(limiter.isBusy)
    executeRequest(0.millis, TooManyRequests)
    assert(limiter.isBusy)
    a await 9.seconds
    assert(!limiter.isBusy)
  }

  "Stream" in {
    implicit val x = JsonSeqStreamSupport
    implicit val y = jsonSeqMarshaller[Json]
    val n = 10
    val duration = 30.millis
    val source: Source[Json, NotUsed] = Source(1 to n) map { o ⇒ blocking(sleep(duration.toMillis)); Json.fromInt(o) }
    val route = complete(source)

    val longRequest = Future {
      val t = now
      Get() ~> limiter(route) ~> check {
        assert(limiter.isBusy)
        assert(now - t < n * duration)

        assert(status == OK)  // Seems to wait for response
        response.discardEntityBytes().future await 9.seconds
        assert(now - t >= n * duration)
        assert(!limiter.isBusy)  // Maybe too early?
      }
    }
    waitForCondition(1.s, 1.ms)(limiter.isBusy)
    assert(limiter.isBusy)
    executeRequest(0.millis, TooManyRequests)

    sleep(20.ms)
    executeRequest(0.millis, TooManyRequests)
    assert(limiter.isBusy)

    longRequest await 9.seconds
    assert(!limiter.isBusy)
    executeRequest(0.millis, OK)
  }

  private def executeRequest(duration: Duration, expectedStatus: StatusCode): Unit = {
    Get() ~> limiter(sleepingRoute(duration)) ~> check {
      assert(status == expectedStatus)
      if (expectedStatus == OK) {
        assert(responseAs[String] == "RESPONSE")
      }
    }
  }
}
