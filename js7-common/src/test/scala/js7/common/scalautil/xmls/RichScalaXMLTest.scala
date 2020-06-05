package js7.common.scalautil.xmls

import js7.common.scalautil.xmls.RichScalaXML.RichElem
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class RichScalaXMLTest extends AnyFreeSpec {

  "attributeText" in {
    val e = <a b="B"/>
    e.attributeText("b") should equal ("B")
    e.attributeText("x") should equal ("")
  }
}
