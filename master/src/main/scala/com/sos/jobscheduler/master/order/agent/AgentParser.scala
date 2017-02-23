package com.sos.jobscheduler.master.order.agent

import com.sos.jobscheduler.common.scalautil.xmls.ScalaXMLEventReader
import com.sos.jobscheduler.data.engine2.agent.AgentPath
import javax.xml.transform.Source

/**
  * @author Joacim Zschimmer
  */
object AgentParser {

  def parseXml(agentPath: AgentPath, source: Source): Agent =
    ScalaXMLEventReader.parseDocument(source) { eventReader ⇒
      import eventReader._

      eventReader.parseElement("agent") {
        Agent(agentPath, attributeMap("uri"))
      }
    }
}
