package js7.controller.agent

import io.circe.Json
import js7.base.circeutils.CirceUtils._
import js7.base.data.ByteArray
import js7.base.problem.Checked
import js7.common.scalautil.xmls.XmlSources.simpleByteArraySource
import js7.core.item.InventoryItemReader
import js7.data.agent.{AgentRef, AgentRefId}
import js7.data.item.SourceType

/**
  * @author Joacim Zschimmer
  */
object AgentRefReader extends InventoryItemReader
{
  val companion = AgentRef

  def read(agentRefId: AgentRefId, source: ByteArray) = {
    case t: SourceType.JsonLike =>
      readAnonymousJsonLike(t, source).map(_ withId agentRefId)

    case SourceType.Xml =>
      AgentRefXmlParser.parseXml(agentRefId, simpleByteArraySource(source))
  }

  def convertFromJson(json: Json): Checked[AgentRef] =
    json.checkedAs[AgentRef]
}
