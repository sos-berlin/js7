package com.sos.scheduler.engine.common.sprayutils

import akka.util.ByteString
import com.sos.scheduler.engine.common.scalautil.xmls.SafeXML
import com.sos.scheduler.engine.common.sprayutils.XmlStringTest._
import com.sos.scheduler.engine.common.xml.XmlUtils.removeXmlProlog
import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets.{ISO_8859_1, UTF_16, UTF_8}
import org.scalatest.FreeSpec
import spray.http.HttpCharsets.{`ISO-8859-1`, `UTF-8`}
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes._
import spray.http.StatusCodes.OK
import spray.http.{ContentType, HttpEntity}
import spray.httpx.marshalling._
import spray.httpx.unmarshalling._
import spray.routing.Directives._
import spray.routing.Route
import spray.testkit.ScalatestRouteTest

/**
 * @author Joacim Zschimmer
 */
final class XmlStringTest extends FreeSpec with ScalatestRouteTest {

  private val testString = "<a><Ä/></a>"
  private val xmlString = XmlString(testString)

  "Marshal as application/xml" in {
    val entity = HttpEntity(`application/xml` withCharset `UTF-8`, testString.getBytes(UTF_8))
    assert(marshal(xmlString) == Right(entity))
  }

  for (charset ← List(`UTF-8`, `ISO-8859-1`)) {
    s"Unmarshal application/xml; charset=$charset" in {
      val entity = HttpEntity(`application/xml` withCharset charset, testString.getBytes(charset.nioCharset))
      assert(entity.as[XmlString] == Right(xmlString))
    }

    s"Unmarshal text/xml; charset=$charset" in {
      val entity = HttpEntity(`text/xml` withCharset charset, testString.getBytes(charset.nioCharset))
      assert(entity.as[XmlString] == Right(xmlString))
    }
  }

  "Unmarshal application/xml with default UTF-8" in {
    val entity = HttpEntity(`application/xml`, testString.getBytes(UTF_8))
    assert(entity.as[XmlString] == Right(xmlString))
  }

  for ((encodingXml, encoding) ← List("UTF-16" → UTF_16, "ISO-8859-1" → ISO_8859_1, "UTF-8" → UTF_8)) {
    s"Unmarshal application/xml with <?xml enoding='$encodingXml'?>" in {
      val withPrologXmlString = s"<?xml version='1.0' encoding='$encodingXml'?>$testString"
      val entity = HttpEntity(`application/xml`, withPrologXmlString.getBytes(encoding))
      assert(entity.as[XmlString] == Right(xmlString))  // Prolog removed (?)
    }
  }

  "Unmarshal text/xml" in {
    val entity = HttpEntity(`text/xml`, testString.getBytes(ISO_8859_1))
    assert(entity.as[XmlString] == Right(xmlString))
  }

  "Web service" - {
    "Simple Post" in {
      Post("/", xmlString) ~> testRoute(testString) ~> check {
        assert(status == OK)
        assert(contentType == `application/xml`.withCharset(`UTF-8`))
        assert(entity.data.asString == testString)
      }
    }

    for (testContentType ← List(
      `application/xml` withCharset `UTF-8`,
      `application/xml` withCharset `ISO-8859-1`,
      `text/xml` withCharset `UTF-8`,
      `text/xml` withCharset `ISO-8859-1`))
    {
      s"$testContentType" in {
        val xmlBytes = testString.getBytes(testContentType.definedCharset.get.nioCharset)
        val entity = HttpEntity(testContentType, xmlBytes)
        addHeader(Accept(testContentType.mediaType)).apply(Post("/", entity)) ~>
          testRoute(testString) ~>
          check {
            assert(status == OK)
            assert(contentType.mediaType == testContentType.mediaType)
            assert(Set(testContentType.charset, `UTF-8`) contains contentType.charset)  // The marshaller may enforce UTF-8
            assert(entity.asString == testString)
          }
      }
    }

    "application/xml without charset, XML with prolog" - {
      for (testCharset ← List(`UTF-8`, `ISO-8859-1`)) s"$testCharset" in {
        val myXml = s"<?xml version='1.0' encoding='${testCharset.value}'?><ä>  <ß>å </ß></ä>"
        val xmlBytes = myXml.getBytes(testCharset.nioCharset)
        assert(SafeXML.loadString(myXml) == SafeXML.load(new ByteArrayInputStream(xmlBytes)))
        val myEntity = HttpEntity(ContentType(`application/xml`, definedCharset = None), ByteString(xmlBytes))
        Post("/", myEntity) ~>
          testRoute(myXml) ~>
          check {
            assert(status == OK)
            assert(entity.asString == removeXmlProlog(myXml))
          }
      }
    }
  }
}

private object XmlStringTest {
  import org.scalatest.Assertions._

  private def testRoute(expected: String): Route =
    entity(as[XmlString]) { xmlString ⇒
      assert(xmlString.string == removeXmlProlog(expected))
      complete(xmlString)
    }
}
