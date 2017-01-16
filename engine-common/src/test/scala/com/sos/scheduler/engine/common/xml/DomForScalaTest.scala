package com.sos.scheduler.engine.common.xml

import com.sos.scheduler.engine.common.xml.DomForScala._
import com.sos.scheduler.engine.common.xml.DomForScalaTest._
import com.sos.scheduler.engine.common.xml.CppXmlUtils.loadXml
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class DomForScalaTest extends FreeSpec {

  "childElements" in {
    testChildElements("<root><a/>x<b><bb/></b>x<c/></root>", "a", "b", "c")
    testChildElements("<root>xx<a/>x<b><bb/></b>x<c/>xx</root>", "a", "b", "c")
  }

  "Eleent.childElementsByName" in {
    assert((loadXml("<root><a/>x<b/><a>AA</a></root>").getDocumentElement / "a" map { _.getTextContent }) == Vector("", "AA"))
  }

  "NodeList.childElementsByName" in {
    val x =
      <root>
        <a/>
        <a>
          <x>X1</x>
          <x>X2</x>
        </a>
        <c>
          <x>?</x>
        </c>
        <a>
          <x>X3</x>
        </a>
      </root>
    assert((loadXml(x.toString).getDocumentElement / "a" / "x" map { _.getTextContent }) == Vector("X1", "X2", "X3"))
  }

  "NodeList.toSeq" in {
    val a = loadXml("<a><b/><c/></a>").getDocumentElement
    assertResult(List("b", "c")) { a.getChildNodes.toSeq map { _.getLocalName }}
    testChildElements("<root><a/>x<b><bb/></b>x<c/></root>", "a", "b", "c")
    testChildElements("<root>xx<a/>x<b><bb/></b>x<c/>xx</root>", "a", "b", "c")
  }
}

object DomForScalaTest {

  private def testChildElements(xml: String, expectedNames: String*): Unit = {
    assertResult(expectedNames) {
      loadXml(xml).getDocumentElement.childElements map { _.getTagName }
    }
  }
}
