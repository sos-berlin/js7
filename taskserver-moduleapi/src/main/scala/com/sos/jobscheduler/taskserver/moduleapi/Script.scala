package com.sos.jobscheduler.taskserver.moduleapi

import com.sos.jobscheduler.base.generic.IsString
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXMLEventReader
import com.sos.jobscheduler.common.utils.Strings.TruncatedString

/**
 * @author Joacim Zschimmer
 */
final case class Script(string: String) extends IsString {
  override def toString = {
    val s = string.truncateWithEllipsis(50)
    s"Script($s)"
  }
}

object Script {
  val Empty = new Script("")

  def parseXmlString(xmlString: String): Script =
    ScalaXMLEventReader.parseString(xmlString) { eventReader ⇒
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
