package com.sos.jobscheduler.common.akkahttp

import akka.actor.ActorSystem
import akka.http.scaladsl.marshalling.{Marshalling, ToEntityMarshaller}
import akka.http.scaladsl.model.MediaTypes._
import akka.http.scaladsl.model.{ContentType, HttpEntity, MessageEntity}
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import akka.stream.ActorMaterializer
import com.sos.jobscheduler.common.akkahttp.JsObjectMarshallers._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import java.nio.charset.StandardCharsets._
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import spray.json._

/**
 * @author Joacim Zschimmer
 */
final class JsObjectMarshallersTest extends FreeSpec with BeforeAndAfterAll {

  private val jsObject = JsObject("a" → JsString("Ä"))
  private val entity = HttpEntity(`application/json`, jsObject.compactPrint.getBytes(UTF_8))
  private lazy val actorSystem = ActorSystem("JsObjectMarshallersTest")
  private implicit lazy val materializer = ActorMaterializer()(actorSystem)

  override def afterAll() = {
    actorSystem.terminate()
    super.afterAll()
  }

  "Marshal application/json" in {
    assert((marshal(jsObject, `application/json`) await 10.s) == entity)
  }

  "Unmarshal application/json" in {
    assert((unmarshal[JsObject](entity) await 10.s) == jsObject)
  }

  private def marshal[A: ToEntityMarshaller](a: A, expectedContentType: ContentType): Future[HttpEntity] =
    for (marshallings ← implicitly[ToEntityMarshaller[A]].apply(a)) yield {
      assert(marshallings.size == 1)
      marshallings.head match {
        case o: Marshalling.WithFixedContentType[MessageEntity] if o.contentType == expectedContentType ⇒
          o.marshal()
      }
    }

  private def unmarshal[A: FromEntityUnmarshaller](entity: HttpEntity): Future[A] =
    implicitly[FromEntityUnmarshaller[A]].apply(entity)
}
