package com.sos.scheduler.engine.http.server.heartbeat

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.io.{IO, Tcp}
import akka.pattern.ask
import akka.util.Timeout
import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.Futures._
import com.sos.scheduler.engine.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.sprayutils.SprayUtils.pathSegments
import com.sos.scheduler.engine.common.sprayutils.YamlJsonConversion.ToYamlString
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.WaitForCondition.waitForCondition
import com.sos.scheduler.engine.common.time.timer.TimerService
import com.sos.scheduler.engine.common.utils.Exceptions.repeatUntilNoException
import com.sos.scheduler.engine.common.utils.FreeTcpPortFinder.findRandomFreeTcpPort
import com.sos.scheduler.engine.http.client.heartbeat.HeartbeatRequestor.HttpRequestTimeoutException
import com.sos.scheduler.engine.http.client.heartbeat.{HeartbeatId, HeartbeatRequestor, HttpHeartbeatTiming}
import com.sos.scheduler.engine.http.server.heartbeat.HeartbeatIT._
import java.time.Duration
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.collection.mutable
import scala.concurrent._
import scala.concurrent.duration.DurationInt
import spray.can.Http
import spray.client.pipelining._
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes._
import spray.http.StatusCodes.OK
import spray.http._
import spray.httpx.SprayJsonSupport._
import spray.httpx.unmarshalling._
import spray.json.DefaultJsonProtocol._
import spray.routing.{HttpServiceActor, Route}

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class HeartbeatIT extends FreeSpec with BeforeAndAfterAll {

  private implicit val askTimeout = AskTimeout
  private implicit val dataJsonFormat = Data.jsonFormat
  private val idempotenceScopes = Iterator from 1
  private implicit lazy val actorSystem = ActorSystem(getClass.getSimpleName)
  import actorSystem.dispatcher
  private implicit lazy val timerService = TimerService(idleTimeout = Some(10.s))
  private lazy val (baseUri, webService) = startWebServer()

  override protected def beforeAll() = {
    super.beforeAll()
    // Warm-up
    autoClosing(new HeartbeatRequestor(HttpHeartbeatTiming(period = 50.ms, timeout = 150.ms))) { heartbeatRequestor ⇒
      val runId = idempotenceScopes.next()
      val uri = s"$baseUri/test/" + runId
      Await.ready(heartbeatRequestor.apply(addHeader(Accept(`application/json`)) ~> sendReceive, Post(uri, Data(100.ms.toString))), 10.seconds)
    }
  }

  override protected def afterAll() = {
    timerService.close()
    actorSystem.shutdown()
    super.afterAll()
  }

  "Requests with heartbeat" - {
    addHeartbeatTests(HttpHeartbeatTiming(period = 50.ms, timeout = 5000.ms))
  }

  "Client-side heartbeat" in {
    WebActor.getHeartbeatCount(webService)  // Clear data ???
    val timing = HttpHeartbeatTiming(period = 100.ms, timeout = 5000.ms)
    val duration = timing.period + timing.period / 2
    val runId = idempotenceScopes.next()
    autoClosing(new HeartbeatRequestor(timing)) { heartbeatRequestor ⇒
      val request = Data(duration.toString)
      val response = heartbeatRequestor.apply(addHeader(Accept(`application/json`)) ~> sendReceive, Post(s"$baseUri/test/$runId", request)).await(10.s)
      assert((response.status, response.as[Data]) == ((OK, Right(request.toResponse))))
      assert(WebActor.getHeartbeatCount(webService) == heartbeatRequestor.serverHeartbeatCount)
      assert(heartbeatRequestor.serverHeartbeatCount == 1)
      assert(heartbeatRequestor.clientHeartbeatCount == 0)
      sleep(duration max HeartbeatRequestor.ClientHeartbeatMinimumDelay * 1.5)
      assert(heartbeatRequestor.clientHeartbeatCount == 1)
      sleep(duration max HeartbeatRequestor.ClientHeartbeatMinimumDelay * 1.5)
      assert(Set(2, 3) contains heartbeatRequestor.clientHeartbeatCount)
      heartbeatRequestor.close()  // Stop sending client-side heartbeats
      requireTimerServiceIsEmpty()
      sleep(3 * duration)
      assert(Set(2, 3) contains heartbeatRequestor.clientHeartbeatCount)
    }
    assertServerIsClean(runId)
  }

  "HttpRequestTimeoutException" in {
    val times = HttpHeartbeatTiming(period = 300.ms, timeout = 5000.ms)
    val debug = new HeartbeatRequestor.Debug
    debug.clientTimeout = Some(100.ms)
    val runId = idempotenceScopes.next()
    autoClosing(new HeartbeatRequestor(times, debug = debug)) { heartbeatRequestor ⇒
      val request = Data(200.ms.toString)
      val responseFuture = heartbeatRequestor.apply(addHeader(Accept(`application/json`)) ~> sendReceive, Post(s"$baseUri/test/$runId", request))
      intercept[HttpRequestTimeoutException] { awaitResult(responseFuture, 1.s) }
      assert(WebActor.getHeartbeatCount(webService) == heartbeatRequestor.serverHeartbeatCount)
      assert(heartbeatRequestor.serverHeartbeatCount == 0)
      assert(heartbeatRequestor.clientHeartbeatCount == 0)
    }
    assertServerIsClean(runId)
    requireTimerServiceIsEmpty()
  }

  if (false) "Endurance" - {
    val times = HttpHeartbeatTiming(period = 4.ms, timeout = 100.ms)
    for (i ← 1 to 100000) s"$i" - {
      addHeartbeatTests(times, highestCurrentOperationsMaximum = 10)
    }
  }

  private def addHeartbeatTests(times: HttpHeartbeatTiming, highestCurrentOperationsMaximum: Int = 2): Unit = {
    for ((duration, requestorDelay, heartbeatCountPredicate) ← List[(Duration, Duration, Int ⇒ Unit)](
      (0.s, 0.s, o ⇒ assert(o == 0)),
      (times.period + times.period * 3/4, 2 * times.period, o ⇒ assert(o == 1)),
      (50 * times.period, 0.s, o ⇒ assert(o >= 5)),
      (50 * times.period, randomDuration(2 * times.period), o ⇒ assert(o >= 3))))
    s"operation = ${duration.pretty}, own delay = ${requestorDelay.pretty}" in {
      WebActor.getHeartbeatCount(webService)  // Clear data
      val runId = idempotenceScopes.next()
      val debug = new HeartbeatRequestor.Debug
      debug.heartbeatDelay = requestorDelay
      autoClosing(new HeartbeatRequestor(times, debug)) { heartbeatRequestor ⇒
        val request = Data(s"$duration")
        val response = heartbeatRequestor.apply(addHeader(Accept(`application/json`)) ~> sendReceive, Post(s"$baseUri/test/$runId", request)) await 10.s
        assert(response.status == OK, s": ${response.entity.asString}")
        assert(response.as[Data] == Right(request.toResponse))
        assert(WebActor.getHeartbeatCount(webService) == heartbeatRequestor.serverHeartbeatCount)
        heartbeatCountPredicate(heartbeatRequestor.serverHeartbeatCount)
        assert(heartbeatRequestor.clientHeartbeatCount == 0)
      }
      assertServerIsClean(runId, highestCurrentOperationsMaximum)
      requireTimerServiceIsEmpty()
    }
  }

  private def assertServerIsClean(runId: Int, highestCurrentOperationsMaximum: Int = 2): Unit = {
    repeatUntilNoException(1000.ms, 10.ms) {
      val (currentOperationsMaximum, currentHeartbeatIds) =
        Await.result((webService ? ("GET" → s"$runId")).mapTo[(Int, Set[HeartbeatId])], AskTimeout.duration)
      assert (currentOperationsMaximum <= highestCurrentOperationsMaximum && currentHeartbeatIds.isEmpty)
    }
  }

  private def requireTimerServiceIsEmpty(): Unit = {
    waitForCondition(2.s, 10.ms) { timerService.overview.count == 0 }  // Heartbeat timeout is asynchronously cancelled
    assert(timerService.overview.count == 0, s"${timerService.overview.toFlowYaml}")
  }
}

object HeartbeatIT {
  private implicit val AskTimeout = Timeout(10.seconds)

  private case class Data(string: String) {
    def toResponse = Data(s"$string RESPONSE")
  }
  private object Data {
    implicit val jsonFormat = jsonFormat1(apply)
  }


  private def startWebServer()(implicit actorSystem: ActorSystem, timerService: TimerService): (Uri, ActorRef) = {
    val port = findRandomFreeTcpPort()
    val webService = actorSystem.actorOf(Props { new WebActor })
    Await.result(IO(Http) ? Http.Bind(webService, interface = "127.0.0.1", port = port), AskTimeout.duration) match {
      case _: Http.Bound ⇒
      case o: Tcp.CommandFailed ⇒ throw new RuntimeException(o.toString)
    }
    (Uri(s"http://127.0.0.1:$port"), webService)
  }

  private class WebActor(implicit timerService: TimerService) extends HttpServiceActor {
    private val logger = Logger(getClass)
    private implicit val executionContext: ExecutionContext = context.system.dispatcher
    private var heartbeatCount = 0
    private val idToHeartbeatService = mutable.Map[String, HeartbeatService]()

    def receive = myReceive orElse runRoute(route)

    private def myReceive: Receive = {
      case ("GET", id: String) ⇒ sender() ! ((idToHeartbeatService(id).pendingOperationsMaximum, idToHeartbeatService(id).pendingHeartbeatIds))
      case "GET-HEARTBEATS" ⇒ sender() ! {
        val r = heartbeatCount
        heartbeatCount = 0
        r
      }
    }

    private def route: Route =
      (pathSegments("test") & path(Segment)) { id: String ⇒
        post {
          val heartbeatService = idToHeartbeatService.getOrElseUpdate(id, new HeartbeatService)
          heartbeatService.continueHeartbeat(timeout ⇒ logger.info(s"Client-side heartbeat ${timeout.pretty}")) ~
            entity(as[Data]) { data ⇒
              heartbeatService.startHeartbeat(onHeartbeat = onHeartbeat) { timeout ⇒
                operation(timeout, data)
              }
            }
        }
      }

    private def operation(timeout: Option[Duration], data: Data) = Future {
      logger.info(s"operation timeout=$timeout $data")
      sleep(Duration parse data.string)
      data.toResponse
    }

    private def onHeartbeat(timeout: Duration): Unit = heartbeatCount += 1
  }

  private object WebActor {
    def getHeartbeatCount(actorRef: ActorRef) = Await.result((actorRef ? "GET-HEARTBEATS").mapTo[Int], AskTimeout.duration)
  }
}
