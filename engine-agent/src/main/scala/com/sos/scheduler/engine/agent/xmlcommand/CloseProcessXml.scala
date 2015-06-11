package com.sos.scheduler.engine.agent.xmlcommand

import com.sos.scheduler.engine.agent.data.AgentProcessId
import com.sos.scheduler.engine.agent.data.commands.CloseProcess
import com.sos.scheduler.engine.common.scalautil.Collections.implicits._
import com.sos.scheduler.engine.common.scalautil.xmls.ScalaXMLEventReader
import com.sos.scheduler.engine.common.xml.XmlUtils.xmlStringToBoolean

/**
 * @author Joacim Zschimmer
 */
object CloseProcessXml {

  def parseXml(eventReader: ScalaXMLEventReader): CloseProcess = {
    import eventReader._
    parseElement(CloseProcess.XmlElementName) {
      CloseProcess(
        processId = attributeMap.convert("process_id") { o ⇒ AgentProcessId(o) },
        kill = attributeMap.getConverted("kill")(xmlStringToBoolean) getOrElse false
      )
    }
  }
}
