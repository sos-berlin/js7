package js7.common.scalautil.xmls

import js7.common.scalautil.xmls.SafeXMLTest._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._
import org.xml.sax.SAXParseException

/**
 * @author Joacim Zschimmer
 */
final class SafeXMLTest extends AnyFreeSpec {

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
