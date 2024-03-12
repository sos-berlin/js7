package js7.common.pekkohttp

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.marshalling.Marshal
import org.apache.pekko.http.scaladsl.model.ContentTypes.`text/plain(UTF-8)`
import org.apache.pekko.http.scaladsl.model.MediaTypes.`application/json`
import org.apache.pekko.http.scaladsl.model.StatusCodes.{BadRequest, OK}
import org.apache.pekko.http.scaladsl.model.headers.Accept
import org.apache.pekko.http.scaladsl.model.{ContentTypes, HttpEntity, HttpRequest, HttpResponse, MessageEntity}
import org.apache.pekko.util.ByteString
import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder
import js7.base.circeutils.CirceUtils.*
import js7.base.problem.{Checked, Problem}
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.*
import js7.base.time.ScalaTime.*
import js7.common.pekkohttp.CirceJsonSupport.*
import js7.common.pekkohttp.StandardMarshallers.*
import js7.common.pekkoutils.Pekkos
import js7.common.pekkoutils.Pekkos.newActorSystem
import org.scalatest.BeforeAndAfterAll
import scala.concurrent.ExecutionContext

/**
  * @author Joacim Zschimmer
  */
final class StandardMarshallersTest extends OurTestSuite, BeforeAndAfterAll {

  private given ExecutionContext = ioRuntime.compute

  implicit private lazy val actorSystem: ActorSystem =
    newActorSystem("StandardMarshallersTest", executionContext = executionContext)

  override def afterAll(): Unit =
    Pekkos.terminateAndWait(actorSystem, 10.s)

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
    implicit val encoder: Encoder.AsObject[A] = deriveEncoder

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
