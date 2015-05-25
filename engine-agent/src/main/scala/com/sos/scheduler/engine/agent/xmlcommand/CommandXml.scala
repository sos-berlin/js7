package com.sos.scheduler.engine.agent.xmlcommand

import com.sos.scheduler.engine.agent.data.commands.{CloseProcess, ProcessCommand, StartProcess}
import com.sos.scheduler.engine.common.scalautil.xmls.ScalaXMLEventReader

/**
 * @author Joacim Zschimmer
 */
object CommandXml {
  def parseString(commandString: String): ProcessCommand =
    ScalaXMLEventReader.parseString(commandString)(parseXml)

  private def parseXml(eventReader: ScalaXMLEventReader): ProcessCommand = {
    import eventReader._
    parseStartElementAlternative[ProcessCommand] {
      case StartProcess.XmlElementName ⇒ StartProcessXml.parseXml(eventReader)
      case CloseProcess.XmlElementName ⇒ CloseProcessXml.parseXml(eventReader)
    }
  }
}
