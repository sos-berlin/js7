package js7.common.akkahttp

import akka.NotUsed
import akka.http.scaladsl.model.StatusCodes.{OK, TooManyRequests}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCode}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import akka.stream.scaladsl.Source
import akka.util.ByteString
import io.circe.Json
import js7.base.problem.Problem
import js7.base.time.ScalaTime._
import js7.common.http.JsonStreamingSupport.{JsonSeqStreamingSupport, jsonSeqMarshaller}
import js7.common.scalautil.Futures.implicits._
import js7.common.time.WaitForCondition.waitForCondition
import monix.execution.Scheduler
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._
import scala.concurrent.{Future, blocking}

/**
  * @author Joacim Zschimmer
  */
final class ConcurrentRequestLimiterExclusiveTest extends AnyFreeSpec with ScalatestRouteTest
{
  private val t = 20.ms
  implicit private val routeTestTimeout = RouteTestTimeout(99.s)
  private val concurrentProblem = Problem.pure("CONCURRENT")
  private val testResponse = HttpEntity.Strict(ContentTypes.`text/plain(UTF-8)`, ByteString("RESPONSE"))

  private def sleepingRoute(duration: Duration): Route =
    complete {
      Future {
        blocking {
          Thread.sleep(duration.toMillis)
          testResponse
        }
      }
    }

  "limit=0 reject all (also warm-up for next test)" in {
    val zeroLimiter = new ConcurrentRequestLimiter(limit = 0, concurrentProblem)(Scheduler.global)
    Get() ~> zeroLimiter(sleepingRoute(0.s)) ~> check {
      assert(status == TooManyRequests)
    }
  }

  "limit=1, no delay" - {
    implicit val limiter = new ConcurrentRequestLimiter(limit = 1, concurrentProblem)(Scheduler.global)

    "One request" in {
      executeRequest(0.s, OK)
      executeRequest(0.s, OK)
      executeRequest(0.s, OK)
    }

    "Two concurrent requests" in {
      val a = Future { blocking { executeRequest(10*t, OK) } }
      waitForCondition(9.s, 1.ms)(limiter.isBusy)
      assert(limiter.isBusy)
      executeRequest(0.s, TooManyRequests)
      assert(limiter.isBusy)
      a await 99.s
      assert(!limiter.isBusy)
    }

    "Stream" in {
      implicit val x = JsonSeqStreamingSupport
      implicit val y = jsonSeqMarshaller[Json]
      val n = 10
      val duration = 3*t
      val source: Source[Json, NotUsed] = Source(1 to n) map { o => sleep(duration); Json.fromInt(o) }
      val route = complete(source)

      val longRequest = Future {
        val runningSince = now
        Get() ~> limiter(route) ~> check {
          assert(limiter.isBusy)
          assert(runningSince.elapsed < n * duration)

          assert(status == OK)  // Seems to wait for response
          response.discardEntityBytes().future await 99.s
          assert(runningSince.elapsed >= n * duration)
          waitForCondition(9.s, 1*t)(!limiter.isBusy)
          assert(!limiter.isBusy)
        }
      }
      waitForCondition(9.s, 1.ms)(limiter.isBusy)
      assert(limiter.isBusy)
      executeRequest(0.s, TooManyRequests)

      sleep(2*t)
      executeRequest(0.s, TooManyRequests)
      assert(limiter.isBusy)

      longRequest await 99.s
      assert(!limiter.isBusy)
      executeRequest(0.s, OK)
    }
  }

  "limit=1 with timeout > 0s" - {
    implicit val limiter = new ConcurrentRequestLimiter(limit = 1, concurrentProblem, timeout = 21*t, queueSize = 9)(Scheduler.global)

    "Second concurrent request is delayed" in {
      val runningSince = now
      val a = Future { blocking { executeRequest(10*t, OK) } }  // running from 0 till 100
      assert(waitForCondition(9.s, 1.ms)(limiter.isBusy))
      val b = Future { blocking { executeRequest(10*t, OK) } }  // waits 100ms, running from 100 till 200
      sleep(3*t)
      val c = Future { blocking { executeRequest(10*t, OK) } }  // waits 170ms, running from 200 till 300
      sleep(3*t)
      val d = Future { blocking { executeRequest(10*t, TooManyRequests) } }   // should wait 240ms but times out after 210ms
      a await 99.s
      assert(limiter.isBusy)
      b await 99.s
      assert(runningSince.elapsed >= 19*t)
      c await 99.s
      assert(runningSince.elapsed >= 29*t)
      d await 99.s
    }
  }

  private def executeRequest(duration: Duration, expectedStatus: StatusCode)(implicit limiter: ConcurrentRequestLimiter): Unit =
    Get() ~> limiter(sleepingRoute(duration)) ~> check {
      assert(status == expectedStatus)
      if (expectedStatus == OK) {
        assert(responseAs[String] == "RESPONSE")
      }
    }
}
