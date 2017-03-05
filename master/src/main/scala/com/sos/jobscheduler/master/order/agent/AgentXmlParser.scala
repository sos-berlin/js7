package com.sos.jobscheduler.master.order.agent

import com.sos.jobscheduler.common.scalautil.xmls.ScalaXMLEventReader
import com.sos.jobscheduler.data.agent.AgentPath
import javax.xml.transform.Source

/**
  * @author Joacim Zschimmer
  */
object AgentXmlParser {

  def parseXml(agentPath: AgentPath, source: Source): Agent =
    ScalaXMLEventReader.parseDocument(source) { eventReader ⇒
      import eventReader._

      eventReader.parseElement("agent") {
        Agent(agentPath, attributeMap("uri"))
      }
    }
}
