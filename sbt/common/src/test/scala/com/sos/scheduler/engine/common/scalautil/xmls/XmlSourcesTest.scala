package com.sos.scheduler.engine.common.scalautil.xmls

import com.sos.scheduler.engine.common.scalautil.xmls.XmlSources._
import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets.UTF_8
import java.util.NoSuchElementException
import javax.xml.namespace.QName
import javax.xml.stream.XMLInputFactory
import javax.xml.stream.events.{EndDocument, StartDocument}
import javax.xml.transform.Source
import org.scalatest.FreeSpec
import org.scalatest.Matchers._

/**
 * @author Joacim Zschimmer
 */
final class XmlSourcesTest extends FreeSpec {

  "stringToSource" in {
    check("<A B='1'/>")
  }

  "inputStreamToSource" in {
    check(new ByteArrayInputStream("<A B='1'/>".getBytes(UTF_8)))
  }

  "xmlElemToSource" in {
    check(<A B='1'/>)
  }

  private def check(source: Source): Unit = {
    val r = XMLInputFactory.newInstance().createXMLEventReader(source)
    r.nextEvent().asInstanceOf[StartDocument]
    val e = r.nextEvent().asStartElement
    e.getName.getLocalPart shouldEqual "A"
    e.getAttributeByName(new QName(null, "B")).getValue shouldEqual "1"
    r.nextEvent().asEndElement
    r.nextEvent().asInstanceOf[EndDocument]
    intercept[NoSuchElementException] { r.nextEvent() }
  }
}
