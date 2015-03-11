package com.sos.scheduler.engine.agent.xmlcommand

import com.sos.scheduler.engine.agent.commands.Command
import com.sos.scheduler.engine.common.scalautil.xmls.ScalaXMLEventReader
import java.net.InetAddress

/**
 * @author Joacim Zschimmer
 */
object CommandXml {
  def parseString(clientIPAddress: InetAddress, commandString: String): Command =
    ScalaXMLEventReader.parseString(commandString)(CommandXml.parseXml(clientIPAddress))

  private def parseXml(clientIPAddress: InetAddress)(eventReader: ScalaXMLEventReader): Command = {
    import eventReader._
    parseStartElementAlternative[Command] {
      case "remote_scheduler.start_remote_task" ⇒
        StartRemoteTaskXml.parseXml(clientIPAddress, eventReader)

      case "remote_scheduler.remote_task.close" ⇒
        CloseRemoteTaskXml.parseCommandXml(eventReader)
    }
  }
}
