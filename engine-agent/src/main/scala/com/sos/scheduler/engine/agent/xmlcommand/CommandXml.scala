package com.sos.scheduler.engine.agent.xmlcommand

import com.sos.scheduler.engine.agent.data.commands._
import com.sos.scheduler.engine.common.scalautil.xmls.ScalaXMLEventReader

/**
 * @author Joacim Zschimmer
 */
object CommandXml {
  def parseString(commandString: String): Command =
    ScalaXMLEventReader.parseString(commandString)(parseXml)

  private def parseXml(eventReader: ScalaXMLEventReader): Command = {
    import eventReader._
    parseStartElementAlternative[Command] {
      case StartProcess.XmlElementName ⇒ StartProcessXml.parseXml(eventReader)
      case CloseProcess.XmlElementName ⇒ CloseProcessXml.parseXml(eventReader)
      case SendProcessSignal.XmlElementName ⇒ SendProcessSignalXml.parseXml(eventReader)
      case Terminate.XmlElementName ⇒ TerminateXml.parseXml(eventReader)
    }
  }
}
