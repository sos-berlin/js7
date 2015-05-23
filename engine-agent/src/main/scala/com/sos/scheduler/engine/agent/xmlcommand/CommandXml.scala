package com.sos.scheduler.engine.agent.xmlcommand

import com.sos.scheduler.engine.agent.data.commands.ProcessCommand
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
      case "remote_scheduler.start_remote_task" ⇒
        StartProcessXml.parseXml(eventReader)

      case "remote_scheduler.remote_task.close" ⇒
        CloseProcessXml.parseCommandXml(eventReader)
    }
  }
}
