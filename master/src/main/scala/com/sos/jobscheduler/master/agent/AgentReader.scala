package com.sos.jobscheduler.master.agent

import akka.util.ByteString
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.common.scalautil.xmls.XmlSources.simpleByteStringSource
import com.sos.jobscheduler.core.filebased.FileBasedReader
import com.sos.jobscheduler.data.agent.{Agent, AgentId}
import com.sos.jobscheduler.data.filebased.SourceType
import io.circe.Json

/**
  * @author Joacim Zschimmer
  */
object AgentReader extends FileBasedReader
{
  val companion = Agent

  def read(agentId: AgentId, source: ByteString) = {
    case t: SourceType.JsonLike ⇒
      readAnonymousJsonLike(t, source) map (_ withId agentId)

    case SourceType.Xml ⇒
      AgentXmlParser.parseXml(agentId, simpleByteStringSource(source))
  }

  def convertFromJson(json: Json): Checked[Agent] =
    json.as[Agent].toSimpleChecked
}
