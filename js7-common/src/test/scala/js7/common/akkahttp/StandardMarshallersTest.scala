package js7.common.akkahttp

import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model.ContentTypes.`text/plain(UTF-8)`
import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.StatusCodes.{BadRequest, OK}
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpRequest, HttpResponse, MessageEntity}
import akka.util.ByteString
import io.circe.generic.semiauto.deriveEncoder
import js7.base.circeutils.CirceUtils._
import js7.base.problem.{Checked, Problem}
import js7.base.thread.Futures.implicits._
import js7.base.time.ScalaTime._
import js7.common.akkahttp.StandardMarshallers._
import js7.common.akkautils.Akkas
import js7.common.akkautils.Akkas.newActorSystem
import js7.common.http.CirceJsonSupport._
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * @author Joacim Zschimmer
  */
final class StandardMarshallersTest extends AnyFreeSpec with BeforeAndAfterAll {

  implicit private val actorSystem = newActorSystem("StandardMarshallersTest")

  override def afterAll(): Unit =
    Akkas.terminateAndWait(actorSystem, 10.s)

  "ToEntityMarshaller[Problem], default is text/plain" in {
    val response = Marshal(Problem("PROBLEM")).toResponseFor(HttpRequest()) await 99.s
    assert(response.status == BadRequest)
    assert(response.entity.contentType == `text/plain(UTF-8)`)
    assert(response.entity.toStrict(99.s).await(99.s).data == ByteString("PROBLEM\n"))
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
    assert(entity.toStrict(99.s).await(99.s).data.utf8String == "PROBLEM\n")
  }

  "checkedToResponseMarshaller" - {
    case class A(number: Int)
    implicit val encoder = deriveEncoder[A]

    "Valid" in {
      val response = Marshal(Right(A(7)): Checked[A]).to[HttpResponse] await 99.s
      assert(response.status == OK)
      assert(response.entity.contentType == ContentTypes.`application/json`)
      assert(response.entity.toStrict(99.s).await(99.s).data.utf8String.parseJsonOrThrow == json"""{ "number": 7 }""")
    }

    "Invalid" in {
      val response = Marshal(Left(Problem("PROBLEM")): Checked[A]).to[HttpResponse] await 99.s
      assert(response.status == BadRequest)
      assert(response.entity.contentType == `text/plain(UTF-8)`)
      assert(response.entity.toStrict(99.s).await(99.s).data == ByteString("PROBLEM\n"))
    }
  }
}
