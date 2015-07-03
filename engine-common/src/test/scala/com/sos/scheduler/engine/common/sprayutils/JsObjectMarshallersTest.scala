package com.sos.scheduler.engine.common.sprayutils

import com.sos.scheduler.engine.common.sprayutils.JsObjectMarshallers._
import java.nio.charset.StandardCharsets._
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.http.HttpEntity
import spray.http.MediaTypes._
import spray.httpx.marshalling._
import spray.httpx.unmarshalling._
import spray.json._

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class JsObjectMarshallersTest extends FreeSpec {

  private val jsObject = JsObject("a" → JsString("Ä"))
  private val entity = HttpEntity(`application/json`, jsObject.compactPrint.getBytes(UTF_8))

  "Marshal application/json" in {
    assert(marshal(jsObject) == Right(entity))
  }

  "Unmarshal application/json" in {
    assert(entity.as[JsObject] == Right(jsObject))
  }
}
