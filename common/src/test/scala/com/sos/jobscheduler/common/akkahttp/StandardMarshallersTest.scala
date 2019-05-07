package com.sos.jobscheduler.common.akkahttp

import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model.ContentTypes.`text/plain(UTF-8)`
import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.StatusCodes.{BadRequest, OK}
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpRequest, HttpResponse, MessageEntity}
import akka.stream.ActorMaterializer
import akka.util.ByteString
import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import com.sos.jobscheduler.common.akkautils.Akkas.newActorSystem
import com.sos.jobscheduler.common.http.CirceJsonSupport._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.base.time.ScalaTime._
import io.circe.generic.semiauto.deriveEncoder
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class StandardMarshallersTest extends FreeSpec with BeforeAndAfterAll {

  private val actorSystem = newActorSystem("StandardMarshallersTest")
  private implicit val mat = ActorMaterializer()(actorSystem)

  override def afterAll(): Unit = {
    mat.shutdown()
    actorSystem.terminate()
  }

  "ToEntityMarshaller[Problem], default is text/plain" in {
    val response = Marshal(Problem("PROBLEM")).toResponseFor(HttpRequest()) await 99.s
    assert(response.status == BadRequest)
    assert(response.entity.contentType == `text/plain(UTF-8)`)
    assert(response.entity.toStrict(99.seconds).await(99.s).data == ByteString("PROBLEM\n"))
  }

  "ToEntityMarshaller[Problem], application/json" in {
    val response = Marshal(Problem("PROBLEM")).toResponseFor(HttpRequest(headers = List(Accept(`application/json`)))) await 99.s
    assert(response.status == BadRequest)
    assert(response.entity.contentType == ContentTypes.`application/json`)
    assert(response.entity.asInstanceOf[HttpEntity.Strict].data.utf8String.parseJsonOrThrow ==  // Should be Strict to allow WebLogDirectives direct access
      json"""{ "TYPE": "Problem", "message": "PROBLEM" }""")
  }

  "problemToEntityMarshaller" in {
    val entity = Marshal(Problem("PROBLEM")).to[MessageEntity] await 99.s
    assert(entity.contentType == `text/plain(UTF-8)`)
    assert(entity.toStrict(99.seconds).await(99.s).data.utf8String == "PROBLEM\n")
  }

  "checkedToResponseMarshaller" - {
    case class A(number: Int)
    implicit val encoder = deriveEncoder[A]

    "Valid" in {
      val response = Marshal(Valid(A(7)): Checked[A]).to[HttpResponse] await 99.s
      assert(response.status == OK)
      assert(response.entity.contentType == ContentTypes.`application/json`)
      assert(response.entity.toStrict(99.seconds).await(99.s).data.utf8String.parseJsonOrThrow == json""" { "number": 7 } """)
    }

    "Invalid" in {
      val response = Marshal(Invalid(Problem("PROBLEM")): Checked[A]).to[HttpResponse] await 99.s
      assert(response.status == BadRequest)
      assert(response.entity.contentType == `text/plain(UTF-8)`)
      assert(response.entity.toStrict(99.seconds).await(99.s).data == ByteString("PROBLEM\n"))
    }
  }
}
