package js7.controller.agent

import javax.xml.transform.Source
import js7.base.problem.Checked
import js7.base.web.Uri
import js7.common.scalautil.xmls.ScalaXMLEventReader
import js7.data.agent.{AgentRef, AgentRefId}

/**
  * @author Joacim Zschimmer
  */
object AgentRefXmlParser {

  def parseXml(agentRefId: AgentRefId, source: Source): Checked[AgentRef] =
    Checked.catchNonFatal {
      ScalaXMLEventReader.parseDocument(source) { eventReader =>
        import eventReader._

        eventReader.parseElement("agent") {
          AgentRef(agentRefId, Uri(attributeMap("uri")))
        }
      }
    }
}
