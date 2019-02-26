package com.sos.jobscheduler.master.agent

import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXMLEventReader
import com.sos.jobscheduler.data.agent.{AgentRef, AgentRefId}
import javax.xml.transform.Source

/**
  * @author Joacim Zschimmer
  */
object AgentRefXmlParser {

  def parseXml(agentRefId: AgentRefId, source: Source): Checked[AgentRef] =
    Checked.catchNonFatal {
      ScalaXMLEventReader.parseDocument(source) { eventReader =>
        import eventReader._

        eventReader.parseElement("agent") {
          AgentRef(agentRefId, attributeMap("uri"))
        }
      }
    }
}
