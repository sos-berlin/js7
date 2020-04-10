package com.sos.jobscheduler.master.agent

import com.sos.jobscheduler.base.web.Uri
import com.sos.jobscheduler.common.scalautil.xmls.XmlSources._
import com.sos.jobscheduler.data.agent.{AgentRef, AgentRefPath}
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
