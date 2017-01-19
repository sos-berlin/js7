package com.sos.scheduler.engine.common.xml

import com.sos.scheduler.engine.common.xml.VariableSets.{parseXml, toParamsXmlElem, toXmlElem}
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class VariableSetsTest extends FreeSpec {

  "parseXml" in {
    val x =
      <anything>
        <variable name="A" value="a"/>
        <variable name="B" value="b"/>
      </anything>
    assert(parseXml(x.toString()) == Map("A" → "a", "B" → "b"))
  }

  "toXmlElem" in {
    assert(toXmlElem(Map("A" → "a")) == <sos.spooler.variable_set><variable name="A" value="a"/></sos.spooler.variable_set>)
    assert(toXmlElem(Map()) == <sos.spooler.variable_set/>)
    assert(toXmlElem(Map("A" → "a"), "params", "param") == <params><param name="A" value="a"/></params>)
  }

  "toXmlElem o parseXml" in {
    val m = Map("A" → "B", "b" → "b")
    assert(parseXml(toXmlElem(Map("A" → "B", "b" → "b")).toString()) == m)
  }

  "toParamsXmlElem" in {
    assert(toParamsXmlElem(Map("A" → "a")) == <params><param name="A" value="a"/></params>)
    assert(toParamsXmlElem(Map()) == <params/>)
  }
}
