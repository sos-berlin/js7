package js7.master.agent

import js7.base.web.Uri
import js7.common.scalautil.xmls.XmlSources._
import js7.data.agent.{AgentRef, AgentRefPath}
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AgentRefXmlParserTest extends AnyFreeSpec {

  "AgentRefXmlParser" in {
    val xml = <agent uri="http://localhost"/>
    val id = AgentRefPath("/FOLDER/AGENT") ~ "VERSION"
    assert(AgentRefXmlParser.parseXml(id, xml) == Right(AgentRef(id, Uri("http://localhost"))))
  }
}
