package com.sos.jobscheduler.common.akkahttp

import akka.actor.ActorSystem
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model.StatusCodes.BadRequest
import akka.http.scaladsl.model.{ContentTypes, HttpResponse, MessageEntity}
import akka.stream.ActorMaterializer
import akka.util.ByteString
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.common.akkahttp.AkkaHttpClientUtils._
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import org.scalatest.concurrent.ScalaFutures._
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class StandardMarshallersTest extends FreeSpec with BeforeAndAfterAll {

  private val actorSystem = ActorSystem("StandardMarshallersTest")
  private implicit val mat = ActorMaterializer()(actorSystem)

  override def afterAll(): Unit = {
    mat.shutdown()
    actorSystem.terminate()
  }

  "problemToResponseMarshaller" in {
    val response = Marshal(Problem("PROBLEM")).to[HttpResponse].futureValue
    assert(response.status == BadRequest)
    assert(response.entity.toStrict(9.seconds).futureValue.data == ByteString("PROBLEM"))
  }

  "problemToEntityMarshaller" in {
    val entity = Marshal(Problem("PROBLEM")).to[MessageEntity].futureValue
    assert(entity.contentType == ContentTypes.`text/plain(UTF-8)`)
    assert(entity.utf8StringFuture.futureValue == "PROBLEM")
  }
}
