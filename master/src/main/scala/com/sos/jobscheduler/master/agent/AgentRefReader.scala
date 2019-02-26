package com.sos.jobscheduler.master.agent

import akka.util.ByteString
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.common.scalautil.xmls.XmlSources.simpleByteStringSource
import com.sos.jobscheduler.core.filebased.FileBasedReader
import com.sos.jobscheduler.data.agent.{AgentRef, AgentRefId}
import com.sos.jobscheduler.data.filebased.SourceType
import io.circe.Json

/**
  * @author Joacim Zschimmer
  */
object AgentRefReader extends FileBasedReader
{
  val companion = AgentRef

  def read(agentRefId: AgentRefId, source: ByteString) = {
    case t: SourceType.JsonLike ⇒
      readAnonymousJsonLike(t, source) map (_ withId agentRefId)

    case SourceType.Xml ⇒
      AgentRefXmlParser.parseXml(agentRefId, simpleByteStringSource(source))
  }

  def convertFromJson(json: Json): Checked[AgentRef] =
    json.checkedAs[AgentRef]
}
