package com.sos.scheduler.engine.agent.web.marshal

import java.nio.charset.StandardCharsets.UTF_8
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.http.HttpCharsets.`UTF-8`
import spray.http.HttpEntity
import spray.http.MediaTypes.{`application/xml`, `text/xml`}
import spray.httpx.marshalling._
import spray.httpx.unmarshalling._

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class XmlStringTest extends FreeSpec {

  val xmlString = "<a><Ä/></a>"
  val entityBytes = xmlString.getBytes(UTF_8)

  "Marshal as application/xml" in {
    val entity = HttpEntity(`application/xml`, entityBytes)
    assert(marshal(XmlString(xmlString)) == Right(entity))
  }

  "Unmarshal application/xml" in {
    val entity = HttpEntity(`application/xml`, entityBytes)
    assert(entity.as[XmlString] == Right(XmlString(xmlString)))
  }

  "Unmarshal text/xml" in {
    val entity = HttpEntity(`text/xml` withCharset `UTF-8`, entityBytes)
    assert(entity.as[XmlString] == Right(XmlString(xmlString)))
  }
}
