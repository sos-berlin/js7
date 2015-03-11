package com.sos.scheduler.engine.taskserver.task.common

import com.sos.scheduler.engine.common.scalautil.xmls.ScalaXMLEventReader
import scala.xml.{TopScope, UnprefixedAttribute}

/**
 * @author Joacim Zschimmer
 */
object VariableSets {

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
    xml.Elem(prefix = null, label = elementName, attributes = xml.Null, scope = TopScope, minimizeEmpty = children.isEmpty, child = children.toSeq: _*)
  }

  def parseXml(string: String, groupName: String = "", elementName: String = "variable"): Map[String, String] =
    ScalaXMLEventReader.parseString(string) { eventReader ⇒
      import eventReader._
      val myGroupName = if (groupName.nonEmpty) groupName else peek.asStartElement.getName.getLocalPart
      parseElement(myGroupName) {
        attributeMap.ignore("count")
        parseEachRepeatingElement("variable") {
          attributeMap("name") → attributeMap.getOrElse("value", "")
        }
      }
    }.toMap
}
