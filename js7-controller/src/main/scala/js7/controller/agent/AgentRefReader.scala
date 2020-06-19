package js7.controller.agent

import akka.util.ByteString
import io.circe.Json
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Checked
import js7.common.scalautil.xmls.XmlSources.simpleByteStringSource
import js7.core.filebased.FileBasedReader
import js7.data.agent.{AgentRef, AgentRefId}
import js7.data.filebased.SourceType

/**
  * @author Joacim Zschimmer
  */
object AgentRefReader extends FileBasedReader
{
  val companion = AgentRef

  def read(agentRefId: AgentRefId, source: ByteString) = {
    case t: SourceType.JsonLike =>
      readAnonymousJsonLike(t, source) map (_ withId agentRefId)

    case SourceType.Xml =>
      AgentRefXmlParser.parseXml(agentRefId, simpleByteStringSource(source))
  }

  def convertFromJson(json: Json): Checked[AgentRef] =
    json.checkedAs[AgentRef]
}
