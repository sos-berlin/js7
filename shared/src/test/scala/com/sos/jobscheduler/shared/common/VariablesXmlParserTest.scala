package com.sos.jobscheduler.shared.common

import com.sos.jobscheduler.common.scalautil.xmls.ScalaXMLEventReader
import com.sos.jobscheduler.common.scalautil.xmls.XmlSources._
import com.sos.jobscheduler.common.time.Stopwatch
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class VariablesXmlParserTest extends FreeSpec {

  "variables" in {
    for (variablesXml <- Array(
      <variables>
        <variable name="NAME" value="VALUE"/>
        <variable name="a" value="aa"/>
      </variables>,
      <params>
        <param name="NAME" value="VALUE"/>
        <param name="a" value="aa"/>
      </params>))
    {
      assert(ScalaXMLEventReader.parseDocument(variablesXml)(VariablesXmlParser.parse) ==
        Map(
          "NAME" → "VALUE",
          "a" → "aa"))
    }
  }

  if (sys.props contains "test.speed") "Speed" in {
    val n = 10000
    val xmlString =
      <variables>{
        for (i ← 1 to 10) yield <variable name={s"NAME-$i"} value={"*" * 100}/>
      }</variables>
      .toString
    for (_ ← 1 to 10) {
      Stopwatch.measureTime(n, "job") {
        ScalaXMLEventReader.parseDocument(xmlString)(VariablesXmlParser.parse)
      }
    }
  }
}
