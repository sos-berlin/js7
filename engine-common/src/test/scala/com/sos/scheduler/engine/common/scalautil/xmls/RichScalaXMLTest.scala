package com.sos.scheduler.engine.common.scalautil.xmls

import RichScalaXML.RichElem
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RichScalaXMLTest extends FunSuite {

  test("attributeText") {
    val e = <a b="B"/>
    e.attributeText("b") should equal ("B")
    e.attributeText("x") should equal ("")
  }
}
