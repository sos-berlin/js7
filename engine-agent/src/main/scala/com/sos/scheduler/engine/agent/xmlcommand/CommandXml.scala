package com.sos.scheduler.engine.agent.xmlcommand

import com.sos.scheduler.engine.agent.commands.Command
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
      case "remote_scheduler.start_remote_task" ⇒
        StartProcessXml.parseXml(eventReader)

      case "remote_scheduler.remote_task.close" ⇒
        CloseProcessXml.parseCommandXml(eventReader)
    }
  }
}
