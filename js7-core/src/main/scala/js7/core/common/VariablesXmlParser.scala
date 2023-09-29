package js7.core.common

import javax.xml.stream.events.{Attribute, EndElement}
import js7.common.scalautil.xmls.ScalaXMLEventReader

/**
  * @author Joacim Zschimmer
  */
object VariablesXmlParser {

  def parse(eventReader: ScalaXMLEventReader): Map[String, String] = {
    import eventReader.*

    def parseVariable(): (String, String) = {
      // Low-level StAX to be faster
      var name = ""
      var value = ""
      val iterator = peek.asStartElement.getAttributes.asInstanceOf[java.util.Iterator[Attribute]]
      while iterator.hasNext do {
        val a = iterator.next()
        a.getName.toString match {
          case "name" => name = a.getValue
          case "value" => value = a.getValue
          case attr => throw new IllegalArgumentException(s"Unknown '$attr' attribute in <param> element")
        }
      }
      require(name.nonEmpty, "Attribute name must not be empty")
      name -> value
    }

    def parseVariables(elementName: String): Map[String, String] = {
      parseElement() {
        val builder = Map.newBuilder[String, String]
        while peek.isStartElement do {
          requireStartElement(elementName)
          builder += parseVariable()
          nextEvent()
          eat[EndElement]
        }
        builder.result()
      }
    }

    matchElement {
      case "variables" => parseVariables("variable")
      case "params" => parseVariables("param")
    }
  }
}
