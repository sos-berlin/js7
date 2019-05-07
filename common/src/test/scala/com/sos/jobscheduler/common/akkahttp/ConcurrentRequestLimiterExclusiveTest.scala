package com.sos.jobscheduler.common.akkahttp

import akka.NotUsed
import akka.http.scaladsl.model.StatusCodes.{OK, TooManyRequests}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCode}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import akka.stream.scaladsl.Source
import akka.util.ByteString
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.time.Timestamp.now
import com.sos.jobscheduler.common.akkahttp.JsonStreamingSupport.{JsonSeqStreamingSupport, jsonSeqMarshaller}
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.time.WaitForCondition.waitForCondition
import io.circe.Json
import java.lang.System.currentTimeMillis
import monix.execution.Scheduler
import org.scalatest.FreeSpec
import scala.concurrent.duration._
import scala.concurrent.{Future, blocking}

/**
  * @author Joacim Zschimmer
  */
final class ConcurrentRequestLimiterExclusiveTest extends FreeSpec with ScalatestRouteTest
{
  private val concurrentProblem = Problem.pure("CONCURRENT")
  private val testResponse = HttpEntity.Strict(ContentTypes.`text/plain(UTF-8)`, ByteString("RESPONSE"))

  private def sleepingRoute(duration: Duration): Route = complete {
    Future {
      blocking {
        Thread.sleep(duration.toMillis)
        testResponse
      }
    }
  }

  "limit=0 reject all (also warm-up for next test)" in {
    val zeroLimiter = new ConcurrentRequestLimiter(limit = 0, concurrentProblem)(Scheduler.global)
    Get() ~> zeroLimiter(sleepingRoute(0.millis)) ~> check {
      assert(status == TooManyRequests)
    }
  }

  "limit=1, no delay" - {
    implicit val limiter = new ConcurrentRequestLimiter(limit = 1, concurrentProblem)(Scheduler.global)

    "One request" in {
      executeRequest(0.millis, OK)
      executeRequest(0.millis, OK)
      executeRequest(0.millis, OK)
    }

    "Two concurrent requests" in {
      val a = Future { blocking { executeRequest(100.millis, OK) } }
      waitForCondition(1.s, 1.ms)(limiter.isBusy)
      assert(limiter.isBusy)
      executeRequest(0.millis, TooManyRequests)
      assert(limiter.isBusy)
      a await 99.seconds
      assert(!limiter.isBusy)
    }

    "Stream" in {
      implicit val x = JsonSeqStreamingSupport
      implicit val y = jsonSeqMarshaller[Json]
      val n = 10
      val duration = 30.millis
      val source: Source[Json, NotUsed] = Source(1 to n) map { o => sleep(duration.toMillis); Json.fromInt(o) }
      val route = complete(source)

      val longRequest = Future {
        val t = now
        Get() ~> limiter(route) ~> check {
          assert(limiter.isBusy)
          assert(now - t < n * duration)

          assert(status == OK)  // Seems to wait for response
          response.discardEntityBytes().future await 99.seconds
          assert(now - t >= n * duration)
          waitForCondition(9.seconds, 10.millis)(!limiter.isBusy)
          assert(!limiter.isBusy)
        }
      }
      waitForCondition(1.s, 1.ms)(limiter.isBusy)
      assert(limiter.isBusy)
      executeRequest(0.millis, TooManyRequests)

      sleep(20.ms)
      executeRequest(0.millis, TooManyRequests)
      assert(limiter.isBusy)

      longRequest await 99.seconds
      assert(!limiter.isBusy)
      executeRequest(0.millis, OK)
    }
  }

  "limit=1 with timeout > 0s" - {
    implicit val limiter = new ConcurrentRequestLimiter(limit = 1, concurrentProblem, timeout = 210.millis, queueSize = 9)(Scheduler.global)

    "Second concurrent request is delayed" in {
      val t = currentTimeMillis
      val a = Future { blocking { executeRequest(100.millis, OK) } }  // running from 0 till 100
      assert(waitForCondition(1.s, 1.ms)(limiter.isBusy))
      val b = Future { blocking { executeRequest(100.millis, OK) } }  // waits 100ms, running from 100 till 200
      sleep(30.millis)
      val c = Future { blocking { executeRequest(100.millis, OK) } }  // waits 170ms, running from 200 till 300
      sleep(30.millis)
      val d = Future { blocking { executeRequest(100.millis, TooManyRequests) } }   // should wait 240ms but times out after 210ms
      a await 99.seconds
      assert(limiter.isBusy)
      b await 99.seconds
      assert(currentTimeMillis - t >= 190)
      c await 99.seconds
      assert(currentTimeMillis - t >= 290)
      d await 99.seconds
    }
  }

  private def executeRequest(duration: Duration, expectedStatus: StatusCode)(implicit limiter: ConcurrentRequestLimiter): Unit = {
    implicit val t = RouteTestTimeout(99.seconds)
    Get() ~> limiter(sleepingRoute(duration)) ~> check {
      assert(status == expectedStatus)
      if (expectedStatus == OK) {
        assert(responseAs[String] == "RESPONSE")
      }
    }
  }
}
