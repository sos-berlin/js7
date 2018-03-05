package com.sos.jobscheduler.master.order.agent

import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXMLEventReader
import com.sos.jobscheduler.data.agent.AgentId
import javax.xml.transform.Source

/**
  * @author Joacim Zschimmer
  */
object AgentXmlParser {

  def parseXml(agentId: AgentId, source: Source): Checked[Agent] =
    Checked.catchNonFatal {
      ScalaXMLEventReader.parseDocument(source) { eventReader ⇒
        import eventReader._

        eventReader.parseElement("agent") {
          Agent(agentId, attributeMap("uri"))
        }
      }
    }
}
