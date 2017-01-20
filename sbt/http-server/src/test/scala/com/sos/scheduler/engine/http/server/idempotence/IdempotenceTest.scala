package com.sos.scheduler.engine.http.server.idempotence

import akka.actor.{ActorSystem, Props}
import akka.io.{IO, Tcp}
import akka.pattern.ask
import akka.util.Timeout
import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.sprayutils.Marshalling.marshalToHttpResponse
import com.sos.scheduler.engine.common.sprayutils.SprayUtils.pathSegments
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.timer.TimerService
import com.sos.scheduler.engine.common.utils.FreeTcpPortFinder._
import com.sos.scheduler.engine.http.client.idempotence.IdempotentHeaders.`X-JobScheduler-Request-ID`
import com.sos.scheduler.engine.http.client.idempotence.RequestId
import com.sos.scheduler.engine.http.server.idempotence.IdempotenceTest._
import java.time.Duration
import java.util.concurrent.atomic.AtomicInteger
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.exceptions.TestFailedException
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import spray.can.Http
import spray.client.pipelining._
import spray.http.Uri
import spray.httpx.SprayJsonSupport._
import spray.httpx.unmarshalling._
import spray.json.DefaultJsonProtocol._
import spray.routing.HttpServiceActor

/**
  * @author Joacim Zschimmer
  */
final class IdempotenceTest extends FreeSpec with BeforeAndAfterAll with ScalaFutures {
  private implicit val askTimeout: Timeout = AskTimeout
  private implicit val dataJsonFormat = DataJsonFormat
  private implicit lazy val actorSystem = ActorSystem(getClass.getSimpleName)
  import actorSystem.dispatcher
  private val newRequestId = new RequestId.Generator
  private lazy val idempotence = new Idempotence()(TimerService(idleTimeout = Some(1.s)))
  private lazy val (baseUri, webService) = startWebServer(idempotence)


  override protected def afterAll() = {
    actorSystem.shutdown()
    super.afterAll()
  }

  "Without RequestId" in {
    val pipeline = sendReceive ~> unmarshal[Data]
    val operations = for (_ ← 1 to 3) yield pipeline(Post(s"$baseUri/test", Data(100.ms, "X")))
    val results = Await.result(Future.sequence(operations), AskTimeout.duration)
    assert(results.toSet == Set(Data(100.ms, "1: X RESPONSE"), Data(100.ms, "2: X RESPONSE"), Data(100.ms, "3: X RESPONSE")))
  }

  "With RequestId, idempotent" in {
    val id = newRequestId()
    val operations = for (i ← 1 to 3) yield {
      sleep(10.ms)
      sendReceive.apply(Post(s"$baseUri/test", Data(100.ms, s"$i")) withHeaders List(`X-JobScheduler-Request-ID`(id)))
    }
    val responses = Await.result(Future.sequence(operations), AskTimeout.duration)
    assert((responses map { _.entity.as[Data] }).toSet == Set(Right(Data(100.ms, "4: 1 RESPONSE"))))
  }

  "With late duplicate RequestId after first response, and lifetime" in {
    val id = newRequestId()
    val lifetime = 500.ms
    val operations = for (i ← 1 to 3) yield {
      sleep(100.ms)
      sendReceive.apply(Post(s"$baseUri/test", Data(0.s, s"$i")) withHeaders List(`X-JobScheduler-Request-ID`(id, Some(lifetime))))
    }
    val responses = Await.result(Future.sequence(operations), AskTimeout.duration)
    assert((responses map { _.entity.as[Data] }).toSet == Set(Right(Data(0.s, s"5: 1 RESPONSE"))))
    assert(idempotence.pendingRequestIds == Some(id))
    sleep(lifetime + 200.ms)
    assert(idempotence.pendingRequestIds.isEmpty)
  }

  locally {
    val n = 10000
    for (index ← 1 to 3) {
      s"Brut force test with $n equal simultaneous requests ($index)" in {
        val id = newRequestId()
        val request = Post(s"$baseUri/test/$index", Data(100.ms, "BRUTFORCE")) withHeaders List(`X-JobScheduler-Request-ID`(id))
        val operations = for (_ ← 1 to n) yield sendReceive.apply(request)
        val responses = Await.result(Future.sequence(operations), AskTimeout.duration)
        try assert((responses map { _.entity.as[Data] }).toSet == Set(Right(Data(100.ms, s"${5 + index}: BRUTFORCE RESPONSE"))))
        catch { case t: TestFailedException ⇒
          logger.error(responses map { o ⇒ s"{$o.status} ${o.entity.as[Data]}" } mkString "\n")
          throw t
        }
      }
    }
  }

  "8 different operations have been executed" in {
    whenReady(webService ? "operationCount") { n ⇒ assert(n == 8) }
  }
}

object IdempotenceTest {
  private val logger = Logger(getClass)
  private implicit val AskTimeout: Timeout = Timeout(30.seconds)

  private def startWebServer(idempotentRequestService: Idempotence)(implicit actorSystem: ActorSystem) = {
    val port = findRandomFreeTcpPort()
    val webService = actorSystem.actorOf(Props { new WebActor(idempotentRequestService) })
    Await.result(IO(Http) ? Http.Bind(webService, interface = "127.0.0.1", port = port), AskTimeout.duration) match {
      case _: Http.Bound ⇒
      case o: Tcp.CommandFailed ⇒ throw new RuntimeException(o.toString)
    }
    (Uri(s"http://127.0.0.1:$port"), webService)
  }

  private case class Data(duration: Duration, string: String) {
    def toResponse = Data(duration, s"$string RESPONSE")
  }
  private implicit val DataJsonFormat = jsonFormat2(Data)

  final class WebActor(idempotence: Idempotence) extends HttpServiceActor {
    private implicit val executionContext: ExecutionContext = context.system.dispatcher
    private val operationCount = new AtomicInteger

    def receive = myReceive orElse runRoute(route)

    def myReceive: Receive = {
      case "operationCount" ⇒ sender() ! operationCount.get
    }

    private def route =
      pathSegments("test") {
        post {
          entity(as[Data]) { data ⇒
            idempotence {
              operation(data) map marshalToHttpResponse
            }
          }
        }
      }

    private def operation(data: Data) = Future {
      val i = operationCount.incrementAndGet()
      sleep(data.duration)
      data.copy(string = s"$i: ${data.string} RESPONSE")
    }
  }
}
