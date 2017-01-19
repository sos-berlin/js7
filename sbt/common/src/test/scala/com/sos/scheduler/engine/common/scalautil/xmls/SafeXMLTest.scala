package com.sos.scheduler.engine.common.scalautil.xmls

import com.sos.scheduler.engine.common.scalautil.xmls.SafeXMLTest._
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.xml.sax.SAXParseException

/**
 * @author Joacim Zschimmer
 */
final class SafeXMLTest extends FreeSpec {

  "xml.XML.load allows DOCTYPE" in {
    assertResult(<a>BAD</a>) { xml.XML.loadString(DoctypeXml) }
  }

  "SafeXML.load rejects DOCTYPE" in {
    assertResult(<a>OKAY</a>) { SafeXML.loadString("<a>OKAY</a>") }
    intercept[SAXParseException] { SafeXML.loadString(DoctypeXml) }
      .getMessage should include ("http://apache.org/xml/features/disallow-doctype-decl")
  }
}

private object SafeXMLTest {
  private val DoctypeXml = "<?xml version='1.0'?><!DOCTYPE test [ <!ENTITY test 'BAD'> ]><a>&test;</a>"
}
