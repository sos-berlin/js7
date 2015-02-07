package com.sos.scheduler.engine.common.scalautil.xmls

import java.util.NoSuchElementException
import javax.xml.namespace.QName
import javax.xml.stream.XMLInputFactory
import javax.xml.stream.events.{EndDocument, StartDocument}
import org.scalatest.FreeSpec
import org.scalatest.Matchers._

/**
 * @author Joacim Zschimmer
 */
final class XmlElemSourceTest extends FreeSpec {
  "XmlElemSource" in {
    val source = XmlElemSource(<A B='1'/>)
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
