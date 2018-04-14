package com.sos.jobscheduler.taskserver.moduleapi

import com.sos.jobscheduler.base.generic.GenericString
import com.sos.jobscheduler.base.utils.Strings.RichString
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXMLEventReader
import com.sos.jobscheduler.common.scalautil.xmls.XmlSources.stringToSource

/**
 * @author Joacim Zschimmer
 */
final case class Script(string: String) extends GenericString {
  override def toString = {
    val s = string.truncateWithEllipsis(50)
    s"Script($s)"
  }
}

object Script {
  val Empty = new Script("")

  def parseXmlString(xmlString: String): Script =
    ScalaXMLEventReader.parseDocument(stringToSource(xmlString)) { eventReader ⇒
      import eventReader._
      val parts = parseElement("source") {
        parseEachRepeatingElement("source_part") {
          val lineNr = attributeMap.get("linenr")  // TODO Die Zeilennummer an die ScriptingEngine übergeben
          eatText()
        }
      }
      Script(parts mkString "")
    }
}
