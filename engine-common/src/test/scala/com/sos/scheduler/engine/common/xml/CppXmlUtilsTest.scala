package com.sos.scheduler.engine.common.xml

import com.sos.scheduler.engine.common.scalautil.xmls.ScalaStax.getCommonXMLInputFactory
import com.sos.scheduler.engine.common.xml.CppXmlUtils.{domElementToStaxSource, _}
import com.sos.scheduler.engine.common.xml.CppXmlUtilsTest._
import java.nio.charset.StandardCharsets.{ISO_8859_1, US_ASCII, UTF_16BE, UTF_8}
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
final class CppXmlUtilsTest extends FreeSpec {

  "domElementToStaxSource" in {
    val domElement = loadXml("<a/>").getDocumentElement
    for (_ <- 1 to 2)
      getCommonXMLInputFactory().createXMLEventReader(domElementToStaxSource(domElement))
  }

  "sanitize" in {
    val original = ((0 to 300) map { _.toChar }).mkString
    val sanitized = sanitize(original)
    assert(original.size == sanitized.size)
    val ValidChars = List('\t', '\n', '\r') ++ (' ' to 300.toChar)
    val InvalidChars = ValidChars.toSet -- ValidChars
    for (c ← ValidChars) {
      assert(original(c) == c)
      assert(sanitized(c) == c)
    }
    for (c ← InvalidChars) {
      assert(original(c) == c)
      assert(sanitized(c) == '�')
    }
  }

  "loadXml String" in {
    assertResult("a") { loadXml("<a>A</a>").getDocumentElement.getNodeName }
  }

  "toXmlBytes" in {
    val doc = loadXml("""<a b="B">Ä</a>""")
    assertResult(s"""<?xml version="1.0" encoding="US-ASCII"?><a b="B">&#196;</a>""") {
      val result = toXmlBytes(doc, "ASCII", indent = false)
      new String(result, US_ASCII)
    }
  }

  "toXmlBytes indented" in {
    val doc = loadXml("<a><b>B</b></a>")
    val nl = System.getProperty("line.separator")
    assertResult(s"""<?xml version="1.0" encoding="US-ASCII"?><a>$nl    <b>B</b>$nl</a>$nl""") {
      val result = toXmlBytes(doc, "ASCII", indent = true)
      val resultString = new String(result, US_ASCII)
      resultString.replaceFirst("[?]>\r?\n", "?>")
    }
  }

  "ToXml" in {
    val xml = """<a b="B">Ä</a>"""
    val doc = loadXml(xml)
    assertResult(xml) { toXml(doc.getDocumentElement) }
  }

  "booleanXmlAttribute true" in {
    testBooleanXmlAttribute(List("<a b='1'/>", "<a b='true'/>"), expected = true)
  }

  "booleanXmlAttribute false" in {
    testBooleanXmlAttribute(List("<a b='0'/>", "<a b='false'/>"), expected = false)
  }

  "booleanXmlAttribute default" in {
    val xmls = List("<a/>", "<a b=''/>")
    testBooleanXmlAttribute_default(xmls, default = false)
    testBooleanXmlAttribute_default(xmls, default = true)
  }

  "boolean XmlAttribute exception" in {
    intercept[RuntimeException] {
      val element = loadXml("<a b='x'/>").getDocumentElement
      booleanXmlAttribute(element, "b", default = false)
    }
  }

  "intXmlAttribute" in {
    val element = loadXml("<a b='4711'/>").getDocumentElement
    assertResult(4711) { intXmlAttribute(element, "b", -99) }
    assertResult(-99) { intXmlAttribute(element, "x", -99) }
  }

  "missingIntXmlAttribute" in {
    intercept[RuntimeException] {
      val element = loadXml("<a/>").getDocumentElement
      intXmlAttribute(element, "b")
    }
  }

  "empty IntXmlAttribute" in {
    intercept[Exception] {
      val element = loadXml("<a b=''/>").getDocumentElement
      intXmlAttribute(element, "b")
    }
  }

  "encoding and rawXmlToString" - {
    "encoding default" in {
      val xmlString = "<ö/>"
      val bytes = new String(xmlString).getBytes(UTF_8)
      assertResult(UTF_8) { encoding(bytes) }
      assertResult(xmlString) { rawXmlToString(bytes) }
    }

    for (enc ← List(UTF_8, UTF_16BE, ISO_8859_1)) {
      s"$enc" in {
        val xmlString = new String(s"<?xml version='1.0' encoding='$enc'?><ö/>")
        val bytes = xmlString.getBytes(enc)
        assertResult(enc) { encoding(bytes) }
        assertResult(xmlString) { rawXmlToString(bytes) }
      }
    }
  }
}

private object CppXmlUtilsTest {
  private def testBooleanXmlAttribute(xmls: Seq[String], expected: Boolean): Unit = {
    for (xml <- xmls; element = loadXml(xml).getDocumentElement) {
      assertResult(expected) { booleanXmlAttribute(element, "b", default = false) }
      assertResult(expected) { booleanXmlAttribute(element, "b", default = true) }
    }
  }

  private def testBooleanXmlAttribute_default(xmls: Seq[String], default: Boolean): Unit = {
    for (xml <- xmls; element = loadXml(xml).getDocumentElement) {
      assertResult(default) { booleanXmlAttribute(element, "b", default = default) }
      assertResult(default) { booleanXmlAttribute(element, "b", default = default) }
    }
  }
}
