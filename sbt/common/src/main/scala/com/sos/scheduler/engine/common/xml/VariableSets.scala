package com.sos.scheduler.engine.common.xml

import com.sos.scheduler.engine.common.scalautil.xmls.ScalaXMLEventReader
import javax.xml.stream.XMLEventReader
import scala.xml.{TopScope, UnprefixedAttribute}

/**
 * @author Joacim Zschimmer
 */
object VariableSets {

  private val DefaultElementName = "variable"

  def toParamsXmlElem(variables: Iterable[(String, String)]) = toXmlElem(variables, "params", "param")

  def toXmlElem(
      variables: Iterable[(String, String)],
      elementName: String = "sos.spooler.variable_set",
      subelementName: String = "variable"): xml.Elem = {

    val children = variables map { case (k, v) ⇒
      xml.Elem(
        prefix = null,
        label = subelementName,
        attributes = new UnprefixedAttribute("name", xml.Text(k), new UnprefixedAttribute("value", xml.Text(v), xml.Null)),
        scope = TopScope,
        minimizeEmpty = true)
    }
    xml.Elem(null: String, elementName, xml.Null, TopScope, children.isEmpty, children.toSeq: _*)
  }

  def parseXml(string: String): Map[String, String] = parseXml(string, groupName = "", elementName = DefaultElementName)

  def parseXml(string: String, groupName: String, elementName: String): Map[String, String] =
    ScalaXMLEventReader.parseString(string) { eventReader ⇒ parseXml(eventReader, groupName, elementName) }

  def parseXml(xmlEventReader: XMLEventReader, groupName: String, elementName: String): Map[String, String] = {
    val eventReader = new ScalaXMLEventReader(xmlEventReader)
    import eventReader._
    val myGroupName = if (groupName.nonEmpty) groupName else peek.asStartElement.getName.getLocalPart
    parseElement(myGroupName) {
      attributeMap.ignore("count")
      parseEachRepeatingElement(elementName) {
        attributeMap("name") → attributeMap.getOrElse("value", "")
      }
    } .toMap
  }
}
