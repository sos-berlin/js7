package js7.core.common

import js7.base.time.Stopwatch.measureTime
import js7.common.scalautil.xmls.ScalaXMLEventReader
import js7.common.scalautil.xmls.XmlSources._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class VariablesXmlParserTest extends AnyFreeSpec {

  "variables" in {
    for (variablesXml <- Array(
     """<variables>
          <variable name="NAME" value="VALUE"/>
          <variable name="a" value="aa"/>
        </variables>""",
     """<params>
          <param name="NAME" value="VALUE"/>
          <param name="a" value="aa"/>
        </params>"""))
    {
      assert(ScalaXMLEventReader.parseDocument(variablesXml)(VariablesXmlParser.parse) ==
        Map(
          "NAME" -> "VALUE",
          "a" -> "aa"))
    }
  }

  if (sys.props contains "test.speed") "Speed" in {
    val n = 10000
    val xmlString =
     "<variables>" +
       (1 to 10).map(i => s"""<variable name="NAME-$i" value="${"*" * 100}"/>""").mkString +
       "</variables>"
    for (_ <- 1 to 10) info(
      measureTime(n) {
        ScalaXMLEventReader.parseDocument(xmlString)(VariablesXmlParser.parse)
      }.toString)
  }
}
